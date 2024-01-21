/*
**
** gv.h
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

#ifndef _GV_GV_H_
#define _GV_GV_H_

#define REQUEST_TOGGLE_RESIZE		-9
#define REQUEST_REDISPLAY		-8
#define REQUEST_SETUP			-7
#define REQUEST_NEW_MAGSTEP		-6
#define REQUEST_NEW_PAGEMEDIA		-5
#define REQUEST_NEW_ORIENTATION		-4
#define REQUEST_REOPEN			-3
#define REQUEST_NEW_FILE		-2
#define NO_CURRENT_PAGE			-1 /* NO_CURRENT_PAGE must be -1 */

#define SPM_ALL                         (1<<0)
#define SPM_EVEN                        (1<<1)
#define SPM_ODD                         (1<<2)
#define SPM_SELECTION			(1<<3)
#define SPM_CURRENT			(1<<4)
#define SPM_TOGGLE			(1<<5)
#define SPM_MARK			(1<<6)
#define SPM_UNMARK			(1<<7)

#define PAGE_MODE_INVALID		0
#define PAGE_MODE_ALL			(1<<0)
#define PAGE_MODE_MARKED		(1<<1)
#define PAGE_MODE_CURRENT		(1<<2)

#define CHECK_FILE_DATE			(1<<0)
#define CHECK_FILE_VERSION		(1<<1)

#define SCROLL_MODE_NONE                0
#define SCROLL_MODE_PANNER              (1<<0)
#define SCROLL_MODE_GHOSTVIEW           (1<<1)
#define SCROLL_MODE_SCROLLBAR           (1<<2)

#define FILE_TYPE_PS			0
#define FILE_TYPE_PDF			1

#define SAVE_MODE_NONE                  0
#define SAVE_MODE_FILE                  1
#define SAVE_MODE_PRINTER               2

#define PROCESS_NONE   0
#define PROCESS_NOTIFY 1
#define PROCESS_KILL   2

#define PROCESS_MENU_NONE       0
#define PROCESS_MENU_HIDE       1
#define PROCESS_MENU_SHOW       2
#define PROCESS_MENU_ADD_ENTRY  3
#define PROCESS_MENU_DEL_ENTRY  4
#define PROCESS_MENU_PROGRESS   5

/* orientations below match the definitions in ps.h */
#define O_INVALID         -1000
#define O_NONE            0
#define O_PORTRAIT        1
#define O_LANDSCAPE       2
#define O_SEASCAPE        3
#define O_UPSIDEDOWN      4
#define O_AUTOMATIC       8
#define O_SWAP_LANDSCAPE  1000

#define MEDIA_ID_INVALID  -3
#define MEDIA_ID_AUTO     -2
#define MEDIA_ID_BB       -1

#if 0

#define MEDIA_AUTO        (1<<0)
#define MEDIA_FROM_DOC    (1<<1)
#define MEDIA_INVALID     (1<<2)

#define ENTRY_INVALID     (-2)
#define ENTRY_BB          (-1)
#define ENTRY_AUTO        0  /* must be the correct number of the "Automatic" entry */

#endif

#endif /* _GV_GV_H_ */




