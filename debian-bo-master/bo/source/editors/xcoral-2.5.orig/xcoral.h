/* ########################################################################

				xcoral.h

   File: xcoral.h
   Path: /home/fournigault/c/X11/xcoral-2.31/xcoral.h
   Description: 
   Created: Fri Jan 27 10:41:12 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:41:13 MET 1995
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

#ifndef  _XCORAL_H_
#define  _XCORAL_H_

#include "main_text.h"
#include "handle_key.h"
#include "init_menus.h"

/*
 *	La structure associe a chaque fenetre d'edition.
 */
typedef struct {
	Window		w_frame;	/* La fenetre mere */
	int		width, height;	/* Geometrie */
	Text		*text;		/* La structure Text */
	Buf		*buf;		/* Le buffer */
	ST		*stat;		/* Etat de l'automate */
	MWin		*mwin;		/* La fenetre pour les messages */
	SWin		*scroll;	/* La fenetre de scroll */	
	XYMenu		*menu;		/* Les menus XY */
	XSizeHints	*shints;	/* Les options */
} EdWin;

extern Display	*dpy;		/* Le display */
extern EdWin	*edwin;		/* La fenetre courante */
extern EdWin	*TWin [];		/* La table de fenetres */
extern XContext	EdContext;	/* Le contexte courant */

extern char 	*menu_names [];
extern char 	**item_names [];	
extern void		(**func_names []) ();	/* Callback */

#define	MAXWIN	32	/* Nb max de fenetres d'editions. 32 ou 64 si on veut */
#define CURRENT_VERSION	"xcoral version 2.5 "

#endif /* _XCORAL_H_ */
