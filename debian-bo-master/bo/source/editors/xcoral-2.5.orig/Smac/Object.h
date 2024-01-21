/* ########################################################################

				Object.h

   File: Object.h
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/Object.h
   Description: 
   Created: Tue Feb 21 10:58:16 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 10:58:17 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

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



#ifndef _Object_h
#define _Object_h


/* Objet permet de memoriser tous les types predefinis
   d'objet dont la taille est <= sizeof(void *) c'est a
   dire tous dans le cas actuel.

   A priori Object devrait donc etre une union,
   malheureusement certain compilateur generent
   des codes absurdes lorsque l'on utilise les unions,
   par exemple dans le cas suivant :

		typedef union {
		  int Int;
		} Object;

		Object f(i)
		     int   i;
		{
		  Object result;
		
		  result.Int = i;

		  return result;
		}

   le compilateur officiel pour SUN4 genere un appel fonctionnel
   (a `.stret4') alors qu'il n'y a rien a faire puisque l'on a
   sizeof(Object) == sizeof(int) !
   Le compilateur GNU ne s'egare pas ainsi. mais tous le monde
   ne l'a pas, aussi Object sera un int ou un long suivant les cas,
   sa definition etant faite par word.c */

#include "word.h"

#endif
