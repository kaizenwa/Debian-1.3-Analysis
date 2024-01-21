
/*-
# X-BASED MISSING LINK(tm)
#
#  Mlink.h
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

/* Public header file for Mlink */

#ifndef _XtMlink_h
#define _XtMlink_h

/***********************************************************************
 *
 * Mlink Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNtiles "tiles"
#define XtNfaces "faces"
#define XtNorient "orient"
#define XtNmiddle "middle"
#define XtNmono "mono"
#define XtNbase "base"
#define XtNstart "start"
#define XtNtileColor "tileColor"
#define XtNtileBorder "tileBorder"
#define XtNfaceColor0 "faceColor0"
#define XtNfaceColor1 "faceColor1"
#define XtNfaceColor2 "faceColor2"
#define XtNfaceColor3 "faceColor3"
#define XtNfaceColor4 "faceColor4"
#define XtNfaceColor5 "faceColor5"
#define XtNfaceColor6 "faceColor6"
#define XtNfaceColor7 "faceColor7"
#define XtCUserName "UserName"
#define XtCTiles "Tiles"
#define XtCFaces "Faces"
#define XtCOrient "Orient"
#define XtCMiddle "Middle"
#define XtCMono "Mono"
#define XtCBase "Base"
#define MLINK_RESTORE (-5)
#define MLINK_RESET (-4)
#define MLINK_BLOCKED (-3)
#define MLINK_SPACE (-2)
#define MLINK_IGNORE (-1)
#define MLINK_MOVED 0
#define MLINK_SOLVED 1
#define MLINK_CONTROL 2
#define MLINK_RANDOMIZE 3
#define MLINK_ORIENT 4
#define MLINK_MIDDLE 5
#define MLINK_DEC_X 6
#define MLINK_INC_X 7
#define MLINK_DEC_Y 8
#define MLINK_INC_Y 9
#define MLINK_COMPUTED 10
#define MLINK_UNDO 11

typedef struct _MlinkClassRec *MlinkWidgetClass;
typedef struct _MlinkRec *MlinkWidget;

extern WidgetClass mlinkWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} mlinkCallbackStruct;

#endif /* _XtMlink_h */
/* DON'T ADD STUFF AFTER THIS #endif */
