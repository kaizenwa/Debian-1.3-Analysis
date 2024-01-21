/* ########################################################################

			      ctr_panel.h

   File: ctr_panel.h
   Path: /home/fournigault/c/X11/xcoral-2.31/ctr_panel.h
   Description: 
   Created: Fri Jan 27 10:56:04 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:56:06 MET 1995
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


#ifndef _CTR_PANEL_H_
#define _CTR_PANEL_H_

#include <X11/Xlib.h>

#include "proto_decl.h"

typedef struct _ButtonWindow {
        Window  w;              /* Fenetre associee */
        void    (* f) ();       /* Callback */
}ButtonWindow;

typedef struct _ResourcesButton {
        Pixmap          head_pix;
        Pixmap          queue_pix;
        Pixmap          up_pix;
        Pixmap          down_pix;
        Pixmap          np_pix;
        Pixmap          pp_pix;
        GC              mess_gc;
        XFontStruct     *font;
        unsigned long   fg, bg;
        unsigned long   top_shadow, bot_shadow;
} ResourcesButton;
/*
 * La structure de controle en bas de la fenetre d'edition.
 */
typedef struct _MWin {
        Window          frame;
        Window          w_stat;
        int             stat;
        Window          mess;
        Window          w_mode;
        Mode            *mode; 
        int             twidth, theight;
        Window          up, down, np, pp;
        Window          head, q;
        char            s [256];
        GC              mess_gc;
        XFontStruct     *font;
        unsigned long   fg, bg, red, green;
        unsigned long   top_sh, bot_sh;
} MWin;

#define BUTTON_W        24
#define MW_SPACE        5
#define NB_BUTTONS      6

#define HeightOfMess() ( BUTTON_W + 4 );
#define ExecButtonFunction(text,i) ( bw [i].f ) ( text );

FCT (void, ClearMessageWindow, (MWin *mwin) );
FCT (void, DeleteControlPanel, (MWin *mwin) );
#ifdef __FreeBSD__
FCT (void, DisplayMessage, (MWin *mwin, const char *) );
#else
FCT (void, DisplayMessage, (MWin *mwin, char *) );
#endif
FCT (int, ExposeInControlePanel, (Window w, MWin *mwin) );
FCT (void, InitControlRes, (unsigned long fg, unsigned long bg, unsigned long ts, unsigned long bs) );
FCT (int, IsButtonInControl, (Window w) );
FCT (MWin *, MakeControlPanel, (Window w) );
FCT (void, RefreshWindowMode, (MWin *mwin) );
FCT (void, RefreshWindowStatBuf, (MWin *mwin) );
FCT (void, SetButton, (MWin *mwin) );
FCT (void, SetCtrMode, (MWin *mwin, Mode *mode) );
FCT (void, ShowControlPanel, (MWin *mwin, int width, int height) );
FCT (void, DownButton, (Window window) );
FCT (void, UpButton, (Window window) );

#endif /* _CTR_PANEL_H_ */
