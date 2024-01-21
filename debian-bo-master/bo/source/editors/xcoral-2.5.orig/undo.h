/* ########################################################################

				 undo.h

   File: undo.h
   Path: /home/fournigault/c/X11/xcoral-2.31/undo.h
   Description: 
   Created: Fri Jan 27 11:38:29 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:38:30 MET 1995
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


#ifndef  _UNDO_H_
#define  _UNDO_H_

#include "proto_decl.h"

#define U_DEL	0
#define U_ADD	1

#define U_INSERT	2
#define U_REP	3
#define U_GREP	4
#define U_STD	5

typedef struct ud_i {
	char c;		/* Si un seul caractere */
	char *buf;		/* Si plusieurs caracteres */
	int size;		/* Taille de buf */
	int one_char;	/* True or False*/
	int current_line;	/* Ligne courante avant l'operation */
	int pos;		/* Position a partir de la marge avant l'operation */
	int nb_lines;	/* Nombre de lignes stokees */
	int op;		/* Ajouter ou effacer 1 ou plusieurs caracteres */
	int type;		/* Insert file, replace strings etc... */
	char *str_old;	/* Pour le replace */
	char *str_new;	/* Idem */
	struct ud_i *next;	/* Pour le chainage */
	struct ud_i *prev;	/* Idem */
	
} Undo;

#endif /* _UNDO_H_ */
