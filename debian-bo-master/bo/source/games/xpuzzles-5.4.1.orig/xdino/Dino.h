
/*-
# X-BASED DINOSAUR CUBE
#
#  Dino.h
#
###
#
#  Copyright (c) 1995 - 97	David Albert Bagley, bagleyd@bigfoot.com
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

/* Public header file for Dino */

#ifndef _XtDino_h
#define _XtDino_h

/***********************************************************************
 *
 * Dino Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNmode "mode"
#define XtNorient "orient"
#define XtNmono "mono"
#define XtNface "face"
#define XtNpos "position"
#define XtNdirection "direction"
#define XtNstyle "style"
#define XtNpractice "practice"
#define XtNstart "start"
#define XtNpieceBorder "pieceBorder"
#define XtNfaceColor0 "faceColor0"
#define XtNfaceColor1 "faceColor1"
#define XtNfaceColor2 "faceColor2"
#define XtNfaceColor3 "faceColor3"
#define XtNfaceColor4 "faceColor4"
#define XtNfaceColor5 "faceColor5"
#define XtCUserName "UserName"
#define XtCMode "Mode"
#define XtCOrient "Orient"
#define XtCMono "Mono"
#define XtCFace "Face"
#define XtCPos "Position"
#define XtCDirection "Direction"
#define XtCStyle "Style"

#define DINO_RESTORE (-4)
#define DINO_RESET (-3)
#define DINO_ILLEGAL (-2)
#define DINO_IGNORE (-1)
#define DINO_MOVED 0
#define DINO_CONTROL 1
#define DINO_SOLVED 2
#define DINO_PRACTICE 3
#define DINO_RANDOMIZE 4
#define DINO_DEC 5
#define DINO_ORIENT 6
#define DINO_INC 7
#define DINO_PERIOD2 8
#define DINO_PERIOD3 9
#define DINO_BOTH 10
#define DINO_COMPUTED 11
#define DINO_UNDO 12

typedef struct _DinoClassRec *DinoWidgetClass;
typedef struct _DinoRec *DinoWidget;

extern WidgetClass dinoWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
	int         face, position, direction, style;
} dinoCallbackStruct;

#endif /* _XtDino_h */
/* DON'T ADD STUFF AFTER THIS #endif */
