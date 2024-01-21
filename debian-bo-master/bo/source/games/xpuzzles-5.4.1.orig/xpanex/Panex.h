/*-
# X-BASED PANEX(tm)
#
#  Panex.h
#
###
#
#  Copyright (c) 1996 - 97	David Albert Bagley, bagleyd@bigfoot.com
#
#                   All Rights Reserved
#
#  Permission to use, copy, modify, and distribute this software and
#  its documentation for any purpose and without fee is hereby granted,
#  provided that the above copyright notice appear in all copies and
#  that both that copyright notice and this permission notice appear in
#  supporting documentation, and that the name of the author not be
#  used in advertising or publicity pertaining to distribution of the
#  software without specific, written prior permission.
#
#  This program is distributed in the hope that it will be "playable",
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
*/

/* Public header file for Panex */

#ifndef _XtPanex_h
#define _XtPanex_h

/***********************************************************************
 *
 * Panex Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNtiles "tiles"
#define XtNmode "mode"
#define XtNmono "mono"
#define XtNbase "base"
#define XtNstart "start"
#define XtNstackColor "stackColor"
#define XtNtileColor "tileColor"
#define XtNtileBorder "tileBorder"
#define XtNpyramidColor0 "pyramidColor0"
#define XtNpyramidColor1 "pyramidColor1"
#define XtNdelay "delay"
#define XtCUserName "UserName"
#define XtCTiles "Tiles"
#define XtCMode "Mode"
#define XtCMono "Mono"
#define XtCBase "Base"
#define XtCDelay "Delay"
#define PANEX_RESTORE (-6)
#define PANEX_RESET (-5)
#define PANEX_ILLEGAL (-4)
#define PANEX_BLOCKED (-3)
#define PANEX_SPACE (-2)
#define PANEX_IGNORE (-1)
#define PANEX_MOVED 0
#define PANEX_SOLVED 1
#define PANEX_DEC 2
#define PANEX_INC 3
#define PANEX_MODE 4
#define PANEX_COMPUTED 5
#define PANEX_UNDO 6

typedef struct _PanexClassRec *PanexWidgetClass;
typedef struct _PanexRec *PanexWidget;

extern WidgetClass panexWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} panexCallbackStruct;

#endif /* _XtPanex_h */
/* DON'T ADD STUFF AFTER THIS #endif */
