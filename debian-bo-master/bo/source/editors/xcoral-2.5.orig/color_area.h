/* ########################################################################

			      color_area.h

   File: color_area.h
   Path: /home/fournigault/c/X11/xcoral-2.31/color_area.h
   Description: 
   Created: Fri Jan 27 10:54:45 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:54:46 MET 1995
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


#ifndef _COLOR_AREA_h
#define _COLOR_AREA_h

#include "proto_decl.h"

/*
 * Liste des elements de couleur.
 */
typedef struct _ColorElement {
    int pos;	 		/* La position (Smac) de l'element */
    int len;			/* La longueur de l'element a colorie */
    unsigned long color;	/* La couleur de l'element */
    struct _ColorElement *next; 	/* Le prochain element colorie */
    struct _ColorElement *previous; /* Le precedent element colorie */
} ColorElement;
  


#endif /* _COLOR_AREA_H_ */
