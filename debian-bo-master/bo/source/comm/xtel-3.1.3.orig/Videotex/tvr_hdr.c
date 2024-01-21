/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: tvr_hdr.c,v 1.3 1997/02/21 16:55:58 pierre Exp $";

/*
 * Decodage des headers Teletel vitesse rapide
 */

#ifndef NO_TVR

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>

#include "VideotexP.h"

typedef enum { LectureEntetePhoto, LectureLongueurHeader, LectureTypeHeader, LectureRangeeHeader, LectureColonneHeader } etatLecture;  

static char *entete_photo = "#@\177";

void init_tvr ();
extern void decode_jpeg_tvr();


/*
 * Lecture des types de base: booleen, reel, entier
 */

static Boolean lecture_booleen (w, pb)
VideotexWidget w;
Boolean *pb;
{
    char c;
    int n = 3;
    register VideotexPart *pv = &w->videotex;

    for (;;) {
	if (read (pv->fd_connexion, &c, 1) < 0)
	    return False;
	
	n--;

	switch (n) {
	  case 2 :
	      if (c != 0x45)
		  return False;
	      break;

	    case 1 :
		
		if (c != 1)
		    return False;
	      break;

	    default :
		if (c == 0 || c == 1) {
		    *pb = (Boolean)c;
		    return True;
		}
	      else
		  return False;
	  }
    }
}

static Boolean lecture_reel (w, pd, pn)
VideotexWidget w;
double *pd;
int *pn;
{
    int n, l=0, v=0, signe=1, nb_bits, masque;
    double terme = 1.0;
    char c;
    register VideotexPart *pv = &w->videotex;
    
    *pn = 0;
    *pd = 0.0;
    n = 2;

    for (;;) {
	if (read (pv->fd_connexion, &c, 1) < 0)
	    return False;

	if (!l) {
	    n--;
	    switch (n) {
	      case 1 :
	      
		if (c != 0x42)
		    return False;
		break;
	      
	      case 0 :
		
		l = c;		/* nouvelle longueur */
		nb_bits = 7 * l - 1;
		masque = 1 << nb_bits;
		*pn = l + 2;
		break;

	      default :
		  *pn = 0;
		return False;
	    }
	}
	else {
	    v <<= 7;
	    v |= (c & 0x7f);

	    if (l == 1) {
		/* Signe */
		if (v & masque) {
		    v = -v & ~(masque >> 1);
		    signe = -1;
		}
		masque >>= 1;
	    }

	    if (!--l)
		break;;
	}
    }
    
    /* Decodage de la valeur */
    while (nb_bits--) {
	if (v & masque)
	    *pd += terme;
	terme /= 2.0;
	masque >>= 1;
    }
    
    *pd *= signe;

    return True;
}

static Boolean lecture_entier (w, pi, pn)
VideotexWidget w;
int *pi, *pn;
{
    int n, l=0, l_init, v=0, signe=1;
    char c;
    register VideotexPart *pv = &w->videotex;
    
    *pi = *pn = 0;
    n = 2;

    for (;;) {
	if (read (pv->fd_connexion, &c, 1) < 0)
	    return False;

	if (!l) {
	    n--;
	    switch (n) {
	      case 1 :
	      
		if (c != 0x40)
		    return False;
		break;
	      
	      case 0 :
		
		l = l_init = c;		/* nouvelle longueur */
		*pn = l + 2;
		break;

	      default :
		  *pn = 0;
		return False;
	    }
	}
	else {
	    if (l == l_init && (c & 0x40)) {
		signe = -1;
		c &= 0x3f;
	    }

	    v <<= 7;
	    v |= (c & 0x7f);

	    if (!--l) {
		*pi = (v * signe);
		return True;
	    }
	}
    }
}

/*
 * Lecture des headers
 */

static int lecture_header_non_implementee (w)
VideotexWidget w;
{
    fprintf (stderr, "Header non implementee.\n");
    return 0;
}

static int lecture_header_reset (w)
VideotexWidget w;
{
    Boolean b;

#ifdef DEBUG
    printf ("lecture_header_reset\n");
#endif

    if (lecture_booleen (w, &b) == True) {
	if (b == True)
	    init_tvr (w);

	return 3;
    }
    else
	return 0;
}

static int lecture_header_position_photo (w)
VideotexWidget w;
{
    int n=0, i;
    register VideotexPart *pv = &w->videotex;

    if (lecture_reel (w, &pv->x_photo, &i) == False)
	return 0;
    
    n += i;

    if (lecture_reel (w, &pv->y_photo, &i) == False)
	return 0;

    n += i;

#ifdef DEBUG
    printf ("lecture_header_position_photo %g %g\n", pv->x_photo, pv->y_photo);
#endif

    return n;
}	

static int lecture_header_taille_photo (w)
VideotexWidget w;
{
    int n=0, i;
    register VideotexPart *pv = &w->videotex;

    if (lecture_reel (w, &pv->w_photo, &i) == False)
	return 0;
    
    n += i;

    if (lecture_reel (w, &pv->h_photo, &i) == False)
	return 0;

    n += i;

#ifdef DEBUG
    printf ("lecture_header_taille_photo %g %g\n", pv->w_photo, pv->h_photo);
#endif

    return n;
}

static int lecture_header_position_image (w)
VideotexWidget w;
{
    int n=0, i;
    register VideotexPart *pv = &w->videotex;

    if (lecture_entier (w, &pv->x_image, &i) == False)
	return 0;
    
    n += i;

    if (lecture_entier (w, &pv->y_image, &i) == False)
	return 0;

    n += i;

    if (lecture_reel (w, &pv->offset_x_image, &i) == False)
	return 0;
    
    n += i;

    if (lecture_reel (w, &pv->offset_y_image, &i) == False)
	return 0;

    n += i;

#ifdef DEBUG
    printf ("lecture_header_position_image %d %d %g %g\n", pv->x_image, pv->y_image, pv->offset_x_image, pv->offset_y_image);
#endif

    return n;
}

static int lecture_header_taille_image (w)
VideotexWidget w;
{
    int n=0, i;
    register VideotexPart *pv = &w->videotex;

    if (lecture_entier (w, &pv->w_image, &i) == False)
	return 0;
    
    n += i;

    if (lecture_entier (w, &pv->h_image, &i) == False)
	return 0;

    n += i;

#ifdef DEBUG
    printf ("lecture_header_taille_image %d %d\n", pv->w_image, pv->h_image);
#endif

    return n;
}

Boolean match (fd, s)
int fd;
char *s;
{
    char c;

    while (read (fd, &c, 1) > 0) {
	if (c == *s) {
	    s++;
	    if (!*s) 
		return True;
	}
	else
	    return False;
    }

    return False;
}

int get_header_length (fd)
int fd;
{
    char c;
    int l = 0;

    while (read (fd, &c, 1) > 0) {
	l <<= 5;
	l |= (c & 0x1f);

	if (!(c & 0x20))
	    return l;
    }
}	

void init_tvr (w)
VideotexWidget w;
{
    register int i, j;
    register VideotexPart *pv = &w->videotex;

    for (i = 0 ; i != 5 ; i++)
	for (j = 0 ; j != 5 ; j++)
	    pv->fonctions_lecture_headers[i][j] = lecture_header_non_implementee;

    pv->fonctions_lecture_headers[0][0] = lecture_header_reset;
    pv->fonctions_lecture_headers[1][2] = lecture_header_position_photo;
    pv->fonctions_lecture_headers[1][3] = lecture_header_taille_photo;
    pv->fonctions_lecture_headers[1][4] = lecture_header_position_image;
    pv->fonctions_lecture_headers[2][1] = lecture_header_taille_image;

    pv->x_photo = pv->y_photo = 0.0;
    pv->w_photo = 1.0;
    pv->h_photo = 0.75;
    pv->x_image = pv->y_image = 0;
    pv->offset_x_image = 0.0;
    pv->offset_y_image = 0.75;
    pv->w_image = 320;
    pv->h_image = 250;

    pv->rectangle_photo.x = 0;
    pv->rectangle_photo.y = pv->hauteur_fonte_base;
    pv->rectangle_photo.width = pv->largeur_ecran;
    pv->rectangle_photo.height = pv->hauteur_ecran;
}

Boolean decode_header_tvr (w, ptype)
VideotexWidget w;
int *ptype;
{
    char c;
    etatLecture etat = LectureEntetePhoto;
    int longueur_header, i, j, n;
    register VideotexPart *pv = &w->videotex;

    *ptype = 0;

    if (!match (pv->fd_connexion, "#@\177"))
	return False;

    longueur_header = get_header_length (pv->fd_connexion);
    etat = LectureTypeHeader;

    while (read (pv->fd_connexion, &c, 1) > 0) {
	switch (etat) {

	    case LectureTypeHeader :

	      longueur_header--;
		
		/* Photo header */
		if (c == 0x50 || c == 0x51) 
		    etat = LectureRangeeHeader;
	      /* Photo data */
	      else if (c == 0x52 || c == 0x53) {
		  if (longueur_header) {
		      /* Calcul de la zone d'affichage photographique */
		      pv->rectangle_photo.x = pv->largeur_ecran * (pv->x_photo + pv->offset_x_image) - (pv->petite_fonte ? pv->x_image : pv->x_image << 1);
		      pv->rectangle_photo.y = pv->hauteur_ecran - (pv->largeur_ecran * (pv->y_photo +  pv->offset_y_image) - (pv->petite_fonte ? pv->y_image : pv->y_image << 1));
		      pv->rectangle_photo.width = (pv->petite_fonte ? pv->w_image : pv->w_image << 1);
		      pv->rectangle_photo.height = (pv->petite_fonte ? pv->h_image : pv->h_image << 1);
		      
		      /* Decodage JPEG */
		      pv->longueur_data_jpeg = longueur_header;
		      *ptype = 1;
		  }

		  return True;
	      }
	      else {
		  fprintf (stderr, "decode_header_tvr: type header %02x inconnu\n", c & 255);
		  return False;
	      }
	      

	      break;

	    case LectureRangeeHeader :
		
		i = c - 0x20;
	      longueur_header--;
	      etat = LectureColonneHeader;
	      break;

	    case LectureColonneHeader :

		j = c - 0x30;
	      longueur_header--;
	      n = (*pv->fonctions_lecture_headers[i][j])(w);
	      if (n >= 0) {
		  longueur_header -= n;

		  if (longueur_header == 0)
		      return True;
		  else
		      etat = LectureRangeeHeader;
	      }
	      else {
		  fprintf (stderr, "decode_header_tvr: erreur lecture header [%d][%d]\n", i, j);
		  return False;
	      }

	      break;

	      default :
		  fprintf (stderr, "decode_header_tvr: etat %d inconnu\n", etat);
		  return False;
	  }
    }

    return False;

}

#endif /* NO_TVR */
