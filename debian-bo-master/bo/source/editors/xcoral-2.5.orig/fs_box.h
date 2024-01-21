/* ########################################################################

				fs_box.h

   File: fs_box.h
   Path: /home/fournigault/c/X11/xcoral-2.31/fs_box.h
   Description: 
   Created: Fri Jan 27 11:02:46 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:02:46 MET 1995
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


#ifndef  _FS_BOX_H_
#define  _FS_BOX_H_

#include "proto_decl.h"

#define FSBOX_FONT "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1"
#define TITLE_SIZE 128

#define FB_SPACE  10
#define FB_MAP    1
#define FB_UNMAP  2

#define FB_OK "Ok"
#define FB_CANCEL "Cancel"
#define FB_DIR "Directories"
#define FB_FILES "Files"
#define FB_SELECT "Select"
#define FB_INPUT = "File name : "

#define FS_BUFFER_SIZE	10000

typedef struct _FBox {
    Window frame, title, dir, ctr, ok, cancel;
    Window dirframe, fileframe, dirtitle, filetitle, dirtext, filetext;
    Window mb_frame;
    unsigned long fg, bg, ts, bs, text_fg, text_bg;
    XFontStruct	*font;
    GC gc;
    int	width, height, t_height, b_width, b_height;
    int str_x, str_y;
    Text *text_dir, *text_file;
    char title_text [TITLE_SIZE];
    Buf	*buf_dir, *buf_file;
    MWin *mwin;
    SWin *scroll_dir, *scroll_file;
    int	select_dir, select_file;
    int	m_width, m_height;
    char *dirname;
    Time click_time;
    Time old_click;
    int stat;
    
} FBox;

#define GetFsBoxFrame() (fs_box.frame)

FCT (int, ButtonFsBox, ( XButtonEvent * ev ));
FCT (void, ConfigFsBox, (int width, int height) );
FCT (int, ExposeFsBox, (XEvent *ev) );
FCT (void, InitFsBox, () );
FCT (char *, GetFsBoxInternalDir, () );
FCT (char *, SelectFileFromBox, (char *msg) );

#endif /* _FS_BOX_H_ */
