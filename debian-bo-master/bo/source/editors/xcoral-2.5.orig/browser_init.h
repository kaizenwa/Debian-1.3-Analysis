/* ########################################################################

			     browser_init.h

   File: browser_init.h
   Path: /home/fournigault/c/X11/xcoral-2.31/browser_init.h
   Description: 
   Created: Fri Jan 27 10:48:04 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:48:05 MET 1995
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


#ifndef _BROWSER_INIT_H_
#define _BROWSER_INIT_H_

#include "main_text.h"

#include "proto_decl.h"

#define B_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define B_MAP		1
#define B_UNMAP		2
#define B_ADD		5
#define B_DEL		6

#define DEC_MODE	0

#define B_BUTTON_WIDTH 	60
#define B_INTER		10
#define B_SPACE		5

#define W_CLASS		0
#define W_PARENT	1
#define W_CHILD		2
#define W_METHOD	3
#define W_PROC		4
#define W_FILES		5
#define W_VISIT		6

#define C_FIRST_WIN	4
#define B_NB_WIN	7

#define RECURSIVE	0
#define NO_RECURSIVE	1

#define B_BUFFER_SIZE	5000
#define B_SEARCH "Search"

/*
 * Les Infos des fenetres de texte.
 */
typedef struct _BWin {
	Window	frame, title, main;	/* Les fenetres principales */
	int 	f_width, f_height;	/* Les dimensions */
	int	t_width, t_height;
	int	m_width, m_height;
	int 	x, y;	/* La position */
	Text	*text;	/* Les infos sur le texte */
	Buf	*buf;	/* Le buffer */
	SWin	*scroll;/* Les Infos sur le scroll */
	int 	select;	/* Ligne selectionnee */
}BWin;

/*
 * Le browser.
 */
typedef struct _Browser {
	int		width, height;	/* Larguer hauteur du browser */
	unsigned long 	fg, bg, text_fg, text_bg;/* Les couleurs */
	Window		frame, title, main;	/* Les fenetres principales */
	char 		*title_name;	/* L'objet selectionne */
	Window		dec,add, del, close, edit;	/* Les boutons */
	BWin		tbw [7];	/* Les fenetres de texte */
	Buf 		*filebuf;	/* Pour les noms de fichiers */
	GC		gc;		/* Le contexte graphique */
	XFontStruct 	*font;		/* La fonte */
	int		title_height;	/* La hauteur du titre */
	int		button_height;	/* la hauteur des boutons */
	int             button_width;   /* La largeur des boutons */
	unsigned long	ts, bs;		/* Les couleurs pour le 3D */
	int 		dec_imp_flag;	/* Toggle flag pour les methodes */
	char 		**methods_save;	/* Sauvegarde des divers pointeurs */
	char		**class_save;
	char		**files_save;
	int 		stat;		/* Browser mappe ou non */
	int		visible;	/* Visibilite */
	Mode		*mode;		/* Le mode */
   	char 		dir [256];	/* La directorie courante */
	int		visit_raise;
	Time		click_time;
	Time		old_click;
	int		parse_flag;	/* Recursif ou pas */
	Window		rec;		/* Toggle button */
} Browser;

FCT (void, InitBrowser, () );
FCT (char *, GetBrowserModeName, () );
FCT (void, SetBrowserDir, (char *) );

FCT (void, LoadDir, (char *dir) );
FCT (void, SetInfos, () );
FCT (void, ExtractInfos, (char **names, int type) );
FCT (void, ConfigScroll, (BWin *bw) );

FCT (void, ConfigBrowser, (int width, int height) );
FCT (void, ConfigVisitWindow, (int width, int height) );
FCT (void, DisplayBrowser, () );
FCT (void, UnmapBrowser, () );
FCT (void, SetBrowserMode, (Mode *mode) );
FCT (void, RefreshBrowserControl, () );

#define GetBrowserFrame()	( br.frame )
#define GetBrowserVisit()	( br.tbw[W_VISIT].frame )

/*
 * Engine
 */
#include "result_types.h"
#include "browser_eng.h"

#endif /* _BROWSER_INIT_H_ */
