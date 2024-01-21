/* ########################################################################

			      main_text.h

   File: main_text.h
   Path: /home/fournigault/c/X11/xcoral-2.31/main_text.h
   Description: 
   Created: Fri Jan 27 11:18:39 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:18:41 MET 1995
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


#ifndef  _MAIN_TEXT_H_
#define  _MAIN_TEXT_H_

#include "main_buffer.h"
#include "scroll.h"
#include "mode.h"
#include "ctr_panel.h"
#include "undo.h"
#include "color_area.h"

#include "proto_decl.h"

/*
 *	Le ressources pour les fenetres Texte.
 */
typedef struct _TextResources {
	GC	cgc;	/* Contexte graphique standard */
	GC	igc;	/* Contexte graphique inverse */
	unsigned long fg, bg, top_sh, bot_sh; /* Les couleurs */
	XFontStruct *font;	/* La fonte */
} TextResources;

#define BUILTIN_FUNC 1
#define EXTERN_FUNC 2

/*
 *	Toutes les infos sur le texte.
 */
typedef struct _Text {
	Window window;		/* La fenetre de texte */
	Window w_parent;	/* La fenetre parent */
	int width, height;	/* Les dimensions */
	int lines_in_buf;	/* Nombre de lignes dans le buffer */
	int no_current_line;	/* Numero de la ligne courante */
	int old_current_line;   /* Save */
	int lines_in_page;	/* Nombre de ligne dans la page */
	int n1, n2;		/* Nb de ligne avant/apres le curseur */
	int old_n1,old_n2;      /* Save */
	XFontStruct *font;	/* La fonte utilisee */
	int char_width_ave;	/* Largeur moyenne d'un caractere */
	int font_height;	/* Hauteur de la fonte en pixels */
	GC Cgc;			/* Le context graphique courant */
	GC Igc;			/* Le context graphique pour l'inverse */
	unsigned long fg, bg;	/* Le devant et le fond */
	unsigned long   top_sh, bot_sh; /* Shadow */
	int x_or, y_or;	/* Origine	 */
	int x_pos, y_pos;	/* Position courante dans la fenetre */
	int cursor_height;	/* Hauteur du curseur */
	int cursor_width;	/* Largeur moyenne du curseur */
	int cursor_stat;	/* Etat du curseur */
	int mouse_in;		/* Mouse in the window */
	int sl;			/* Horizontal Scroll */

	char filename [256];
	char current_dir [256];
	int modif;
	int stat;
	int visible;
	
	Buf *buf;	/* Le Buffer */
	SWin *swin;	/* Le scroll */
	MWin *mwin; 	/* La fenetre des messages et bouttons */
	int width_relief;

	struct {
	       int      wline [256];	/* La largeur des lignes */
	       char     *sline [256];	/* La table des lignes */
	       int	hs_delta;
	} page;

	int markline;	/* Le numero de la ligne */
	int markpos;	/* La position dans la ligne */
	
	Mode *current_mode;
	
	char indent[8]; /* Pour les espaces */
	int tab_width;
	int blanc_width;
	
	Undo *udi; /* Pour le Undo */
	int u_index;
	int u_todo;
	
	int win_id; /* Pour les manipulations de fenetres a partir de l'interpreteur */
	int ie_mark;
	
	int shell_id; /* Pour la connection eventuelle a un shell */
	int p_shell [2]; /* Le pipes */
	int p_xc [2];
	int from_shell, to_shell;
	int s_line, s_pos;
	ColorElement *current_ce;
	ColorElement *last_ce;
} Text;


FCT (void, ChangeDir, (Text *text) );
FCT (void, ChangeFont, (Display *dpy, Text *text, char *f) );
FCT (void, ConfigTextAndScroll, (Text *text, int width, int height, int space) );
FCT (void, DeleteText, (Display *dpy, Text *text) );
FCT (void, ExposeTextWindow, (Display *dpy, Text *text, XEvent *ev) );
FCT (void, InitTextRes, (Display *dpy, XFontStruct *font, unsigned long fg, unsigned long bg, unsigned long ts, unsigned long bs) );
FCT (void, KillText, (Display *dpy, Text *text) );
FCT (XFontStruct *, LoadFont, (Display *dpy, char *str) );
FCT (void, LoadMode, (Text *text) );
FCT (Text *, MakeTextWindow, (Display *dpy, Window parent, int x, int y) );
FCT (void, MouseIn, (Text *text) );
FCT (void, MouseOut, (Text *text) );
FCT (void, NewMode, (Text *text) );
FCT (void, RefreshWithNewFont, (Text *text, XFontStruct *font) );
FCT (void, SetCCMode, (Text *text) );
FCT (void, SetCMode, (Text *text) );
FCT (void, SetDefaultMode, (Text *text) );
FCT (void, SetFontText, (Display *dpy, Text *text, XFontStruct *font) );
FCT (void, SetTextModif, (Text *text) );
FCT (void, SetTextSave, (Text *text) );
FCT (void, ShowWindowText, (Display *dpy, Text *text, int width, int height) );
FCT (int, TextInBuf, (Text *text) );
FCT (void, ie_WR_delete, (Text *text) );

FCT (void, DeleteUndo, (Text *text) );
FCT (void, DoUndo, (Text *text) );
FCT (void, DoUndoUndo, (Text *text) );
FCT (void, ResetUndo, (Text *text) );
FCT (void, StoreInUndo, (Text *text, char *s, char *sr, int len, int n, int type) );
FCT (void, SetLatexMode, (Text *text) );
FCT (void, SetHtmlMode, (Text *text) );
FCT (void, SetAdaMode, (Text *text) );
FCT (void, SetPerlMode, (Text *text) );
FCT (void, SetFortranMode, (Text *text) );
FCT (void, SetEdirMode, (Text *text) );
FCT (void, SetshellMode, (Text *text) );

FCT (void, SetColorArea, (Text *text, int start, int end, char *colorname) );
FCT (void, DeleteColorList, (Text *text) );
FCT (void, UpdateColorList, (Text *text, int nbytes) );
FCT (void, ColorLineInPage, (Text *text,  int x, int y, int li, char *s, int n) );
FCT (unsigned long, GetCursorColor, (Text *text) );

#define GetTextWindow(text) 	( text -> window )
#define GetModif(text)		( text -> modif )
#define GetFileName(text)	( text -> filename )
#define SetLineInPage(text,n) 	( text -> lines_in_page = n )
#define GetCursorStat(text)	( text -> cursor_stat )
#define GetScrollStat(text)	( text -> sl )
#define GetNbLinesInBuf(text)	( text -> lines_in_buf )

#define TAB_WIDTH	8
#define INDENT_WIDTH	3
#define	OFF		0
#define ON              1
#define BIZ_CHAR 191

#define	MARGE	10
#define W_SPACE 5

#define CTX_EVAL_EXP 11
#define CTX_FILE 12

#define W_RELIEF	2
#define FREESE		2
#define UNFREESE	3
#define MSET(x,y)	( x |= y )
#define MCLEAR(x,y)	( x &= (~y) )
#define MESSAGE		( 1 << 0 )
#define FIRSTPAGE	( 1 << 1 )
#define LASTPAGE	( 1 << 2 )

#define END_MESS "This is the end... my friend the end"

#define F_KEY	0
#define F_MENU	1

#define FILESELECT	0	
#define KILLBUF		1
#define OPENFILES	2

#endif /* _MAIN_TEXT_H_ */





