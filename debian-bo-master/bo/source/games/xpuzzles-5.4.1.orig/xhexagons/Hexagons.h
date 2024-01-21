
/*-
# X-BASED HEXAGONS
#
#  Hexagons.h
#
###
#
#  Copyright (c) 1994 - 97	David Albert Bagley, bagleyd@bigfoot.com
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

/* Public header file for Hexagons */

#ifndef _XtHexagons_h
#define _XtHexagons_h

/***********************************************************************
 *
 * Hexagons Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNsize "size"
#define XtNcorners "corners"
#define XtNbase "base"
#define XtNpractice "practice"
#define XtNstart "start"
#define XtNtileColor "tileColor"
#define XtNtileBorder "tileBorder"
#define XtCUserName "UserName"
#define XtCSize "Size"
#define XtCCorners "Corners"
#define XtCBase "Base"
#define HEXAGONS_RESTORE (-5)
#define HEXAGONS_RESET (-4)
#define HEXAGONS_BLOCKED (-3)
#define HEXAGONS_SPACE (-2)
#define HEXAGONS_IGNORE (-1)
#define HEXAGONS_MOVED 0
#define HEXAGONS_SOLVED 1
#define HEXAGONS_PRACTICE 2
#define HEXAGONS_RANDOMIZE 3
#define HEXAGONS_DEC 4
#define HEXAGONS_INC 5
#define HEXAGONS_CORNERS 6
#define HEXAGONS_COMPUTED 7
#define HEXAGONS_UNDO 8

typedef struct _HexagonsClassRec *HexagonsWidgetClass;
typedef struct _HexagonsRec *HexagonsWidget;

extern WidgetClass hexagonsWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} hexagonsCallbackStruct;

#endif /* _XtHexagons_h */
/* DON'T ADD STUFF AFTER THIS #endif */
