/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 */

static	char	*rcsid = "$Id: rotate.c,v 143.1 1996/09/16 09:08:55 nau Exp $";

/* functions used to rotate pins, elements ...
 */

#include <stdlib.h>

#include "global.h"

#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "misc.h"
#include "rotate.h"
#include "search.h"
#include "select.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*RotateText(LayerTypePtr, TextTypePtr);
static	void	*RotateElement(ElementTypePtr);
static	void	*RotateElementName(ElementTypePtr);
static	void	RotateArcLowLevel(ArcTypePtr, Position, Position, BYTE);

/* ----------------------------------------------------------------------
 * some local identifiers
 */
static	Position			CenterX,	/* center of rotation */
							CenterY;
static	BYTE				Number;		/* number of rotations */
static	ObjectFunctionType	RotateFunctions = {
	NULL,
	RotateText,
	NULL,
	NULL,
	RotateElement,
	RotateElementName,
	NULL,
	NULL,
	NULL,
	NULL };

/* ---------------------------------------------------------------------------
 * rotates a line in 90 degree steps
 */
void RotateLineLowLevel(LineTypePtr Line, Position X, Position Y, BYTE Number)
{
	ROTATE(Line->Point1.X, Line->Point1.Y, X, Y, Number);
	ROTATE(Line->Point2.X, Line->Point2.Y, X, Y, Number);

}

/* ---------------------------------------------------------------------------
 * rotates a text in 90 degree steps 
 * only the bounding box is rotated, text rotation itself
 * is done by the drawing routines
 */
void RotateTextLowLevel(TextTypePtr Text, Position X, Position Y, BYTE Number)
{
	BYTE	number;

	number = TEST_FLAG(ONSOLDERFLAG, Text) ? (4 -Number) & 0x03 : Number;
	RotateBoxLowLevel(&Text->BoundingBox, X, Y, number);
	ROTATE(Text->X, Text->Y, X, Y, number);

		/* set new direction, 0..3,
		 * 0-> to the right, 1-> straight up,
		 * 2-> to the left, 3-> straight down
		 */
	Text->Direction = ((Text->Direction +Number) & 0x03);
}

/* ---------------------------------------------------------------------------
 * rotates a polygon in 90 degree steps
 */
void RotatePolygonLowLevel(PolygonTypePtr Polygon,
	Position X, Position Y, BYTE Number)
{
	POLYGONPOINT_LOOP(Polygon,
		ROTATE(point->X, point->Y, X, Y, Number);
	);
	RotateBoxLowLevel(&Polygon->BoundingBox, X, Y, Number);
}

/* ---------------------------------------------------------------------------
 * rotates a text object and redraws it
 */
static void *RotateText(LayerTypePtr Layer, TextTypePtr Text)
{
	EraseText(Text);
	RotateTextLowLevel(Text, CenterX, CenterY, Number);
	DrawText(Layer, Text);
	return(Text);
}

/* ---------------------------------------------------------------------------
 * rotates an arc
 */
static void RotateArcLowLevel(ArcTypePtr Arc,
	Position X, Position Y,
	BYTE Number)
{
	Dimension	save;

		/* add Number*90 degrees to the startangle and check for overflow */
	Arc->StartAngle = (Arc->StartAngle +Number*90) % 360;
	ROTATE(Arc->X, Arc->Y, X, Y, Number);

		/* now change width and height */
	if (Number == 1 || Number == 3)
	{
		save = Arc->Width;
		Arc->Width = Arc->Height;
		Arc->Height = save;
	}
}

/* ---------------------------------------------------------------------------
 * rotate an element in 90 degree steps
 */
void RotateElementLowLevel(ElementTypePtr Element,
	Position X, Position Y,
	BYTE Number)
{
	BYTE	number;

		/* solder side objects need a different orientation */
	number = TEST_FLAG(ONSOLDERFLAG, Element) ? (4 -Number) & 0x03 : Number;

		/* the text subroutine decides by itself if the direction
		 * is to be corrected
		 */
	ELEMENTTEXT_LOOP(Element, RotateTextLowLevel(text, X, Y, Number));
	ELEMENTLINE_LOOP(Element, RotateLineLowLevel(line, X, Y, number););
	PIN_LOOP(Element, ROTATE_PIN_LOWLEVEL(pin, X, Y, number););
	PAD_LOOP(Element, ROTATE_PAD_LOWLEVEL(pad, X, Y, number););
	ARC_LOOP(Element, RotateArcLowLevel(arc, X, Y, number););
	ROTATE(Element->MarkX, Element->MarkY, X, Y, number);
	RotateBoxLowLevel(&Element->BoundingBox, X, Y, number);
}

/* ---------------------------------------------------------------------------
 * rotates an element
 */
static void *RotateElement(ElementTypePtr Element)
{
	EraseElement(Element);
	RotateElementLowLevel(Element, CenterX, CenterY, Number);
	DrawElement(Element);
	return(Element);
}

/* ----------------------------------------------------------------------
 * rotates the name of an element
 */
static void *RotateElementName(ElementTypePtr Element)
{
	EraseElementName(Element);
	ELEMENTTEXT_LOOP(Element, RotateTextLowLevel(text,CenterX,CenterY,Number));
	DrawElementName(Element);
	return(Element);
}

/* ---------------------------------------------------------------------------
 * rotates a box in 90 degree steps 
 */
void RotateBoxLowLevel(BoxTypePtr Box,
	Position X, Position Y, BYTE Number)
{
	Position	x1 = Box->X1,
				y1 = Box->Y1,
				x2 = Box->X2,
				y2 = Box->Y2;

	ROTATE(x1, y1, X, Y, Number);
	ROTATE(x2, y2, X, Y, Number);
	Box->X1 = MIN(x1, x2);
	Box->Y1 = MIN(y1, y2);
	Box->X2 = MAX(x1, x2);
	Box->Y2 = MAX(y1, y2);
}

/* ----------------------------------------------------------------------
 * rotates an objects at the cursor position as identified by its ID
 * the center of rotation is determined by the current cursor location
 */
void *RotateObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position X, Position Y, BYTE Steps)
{
		/* setup default  global identifiers */
	CenterX = X;
	CenterY = Y;
	Number = Steps;
	AddObjectToRotateUndoList(Type, Ptr1, Ptr2, Ptr3, CenterX, CenterY, Steps);
	IncrementUndoSerialNumber();
	return(ObjectOperation(&RotateFunctions, Type, Ptr1, Ptr2, Ptr3));
}

