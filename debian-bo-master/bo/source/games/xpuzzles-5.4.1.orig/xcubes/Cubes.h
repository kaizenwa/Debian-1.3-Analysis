
/*-
# X-BASED CUBES
#
#  Cubes.h
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

/* Public header file for Cubes */

#ifndef _XtCubes_h
#define _XtCubes_h

/***********************************************************************
 *
 * Cubes Widget
 *
 ***********************************************************************/

#define XtNselectCallback "selectCallback"
#define XtNuserName "userName"
#define XtNsizeX "sizeX"
#define XtNsizeY "sizeY"
#define XtNsizeZ "sizeZ"
#define XtNbase "base"
#define XtNstart "start"
#define XtNbrickColor "brickColor"
#define XtNbrickBorder "brickBorder"
#define XtCUserName "UserName"
#define XtCSizeX "SizeX"
#define XtCSizeY "SizeY"
#define XtCSizeZ "SizeZ"
#define XtCBase "Base"
#define CUBES_RESTORE (-5)
#define CUBES_RESET (-4)
#define CUBES_BLOCKED (-3)
#define CUBES_SPACE (-2)
#define CUBES_IGNORE (-1)
#define CUBES_MOVED 0
#define CUBES_SOLVED 1
#define CUBES_PRACTICE 2
#define CUBES_RANDOMIZE 3
#define CUBES_DEC_X 4
#define CUBES_INC_X 5
#define CUBES_DEC_Y 6
#define CUBES_INC_Y 7
#define CUBES_DEC_Z 8
#define CUBES_INC_Z 9
#define CUBES_COMPUTED 10
#define CUBES_UNDO 11

typedef struct _CubesClassRec *CubesWidgetClass;
typedef struct _CubesRec *CubesWidget;

extern WidgetClass cubesWidgetClass;

typedef struct {
	XEvent     *event;
	int         reason;
} cubesCallbackStruct;

#endif /* _XtCubes_h */
/* DON'T ADD STUFF AFTER THIS #endif */
