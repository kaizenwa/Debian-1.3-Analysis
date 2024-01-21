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

static	char	*rcsid = "$Id: move.c,v 143.1 1996/09/16 09:08:47 nau Exp $";

/* functions used to move pins, elements ...
 */

#include <stdlib.h>

#include "global.h"

#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "misc.h"
#include "move.h"
#include "mymem.h"
#include "polygon.h"
#include "search.h"
#include "select.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*MoveElementName(ElementTypePtr);
static	void	*MoveElement(ElementTypePtr);
static	void	*MoveVia(PinTypePtr);
static	void	*MoveLine(LayerTypePtr, LineTypePtr);
static	void	*MoveText(LayerTypePtr, TextTypePtr);
static	void	*MovePolygon(LayerTypePtr, PolygonTypePtr);
static	void	*MoveLinePoint(LayerTypePtr, LineTypePtr, PointTypePtr);
static	void	*MovePolygonPoint(LayerTypePtr, PolygonTypePtr, PointTypePtr);
static	void	*MoveLineToLayer(LayerTypePtr, LineTypePtr);
static	void	*MoveTextToLayer(LayerTypePtr, TextTypePtr);
static	void	*MovePolygonToLayer(LayerTypePtr, PolygonTypePtr);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Position			DeltaX,		/* used by local routines as offset */
							DeltaY;
static	LayerTypePtr		Dest;
static	ObjectFunctionType	MoveFunctions = {
	MoveLine,
	MoveText,
	MovePolygon,
	MoveVia,
	MoveElement,
	MoveElementName,
	NULL,
	NULL,
	MoveLinePoint,
	MovePolygonPoint },
							MoveToLayerFunctions = {
	MoveLineToLayer,
	MoveTextToLayer,
	MovePolygonToLayer,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL };

/* ---------------------------------------------------------------------------
 * moves a element by +-X and +-Y
 */
void MoveElementLowLevel(ElementTypePtr Element, Position DX, Position DY)
{
	ELEMENTLINE_LOOP(Element, MOVE_LINE_LOWLEVEL(line, DX, DY););
	PIN_LOOP(Element, MOVE_PIN_LOWLEVEL(pin, DX, DY););
	PAD_LOOP(Element, MOVE_PAD_LOWLEVEL(pad, DX, DY););
	ARC_LOOP(Element, MOVE_ARC_LOWLEVEL(arc, DX, DY););
	ELEMENTTEXT_LOOP(Element, MOVE_TEXT_LOWLEVEL(text, DX, DY););
	MOVE_BOX_LOWLEVEL(&Element->BoundingBox, DX, DY);
	MOVE(Element->MarkX, Element->MarkY, DX, DY);
}

/* ----------------------------------------------------------------------
 * moves all names of an element to a new position
 */
static void *MoveElementName(ElementTypePtr Element)
{
	if (PCB->ElementOn &&
		((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT ||
		PCB->InvisibleObjectsOn))
	{
		EraseElementName(Element);
		ELEMENTTEXT_LOOP(Element, MOVE_TEXT_LOWLEVEL(text, DeltaX, DeltaY));
		DrawElementName(Element);
	}
	else
	{
		ELEMENTTEXT_LOOP(Element, MOVE_TEXT_LOWLEVEL(text, DeltaX, DeltaY));
	}
	return(Element);
}

/* ---------------------------------------------------------------------------
 * moves an element
 */
static void *MoveElement(ElementTypePtr Element)
{
	if (PCB->ElementOn &&
		((TEST_FLAG(ONSOLDERFLAG, Element) != 0) == SWAP_IDENT ||
		PCB->InvisibleObjectsOn))
	{
		EraseElement(Element);
		MoveElementLowLevel(Element, DeltaX, DeltaY);
		DrawElementName(Element);
		DrawElementPackage(Element);
	}
	else
	{
		if (PCB->PinOn)
			EraseElementPinsAndPads(Element);
		MoveElementLowLevel(Element, DeltaX, DeltaY);
	}
	if (PCB->PinOn)
		DrawElementPinsAndPads(Element);
	return(Element);
}

/* ---------------------------------------------------------------------------
 * moves a via
 */
static void *MoveVia(PinTypePtr Via)
{
	if (PCB->ViaOn)
	{
		EraseVia(Via);
		MOVE_VIA_LOWLEVEL(Via, DeltaX, DeltaY);
		DrawVia(Via);
	}
	else
		MOVE_VIA_LOWLEVEL(Via, DeltaX, DeltaY);
	return(Via);
}

/* ---------------------------------------------------------------------------
 * moves a line
 */
static void *MoveLine(LayerTypePtr Layer, LineTypePtr Line)
{
	if (Layer->On)
	{
		EraseLine(Line);
		MOVE_LINE_LOWLEVEL(Line, DeltaX, DeltaY);
		DrawLine(Layer, Line);
	}
	else
		MOVE_LINE_LOWLEVEL(Line, DeltaX, DeltaY);
	return(Line);
}

/* ---------------------------------------------------------------------------
 * moves a text object
 */
static void *MoveText(LayerTypePtr Layer, TextTypePtr Text)
{
	if (Layer->On)
	{
		EraseText(Text);
		MOVE_TEXT_LOWLEVEL(Text, DeltaX, DeltaY);
		DrawText(Layer, Text);
	}
	else
		MOVE_TEXT_LOWLEVEL(Text, DeltaX, DeltaY);
	return(Text);
}

/* ---------------------------------------------------------------------------
 * low level routine to move a polygon
 */
void MovePolygonLowLevel(PolygonTypePtr Polygon,
	Position DeltaX, Position DeltaY)
{
	POLYGONPOINT_LOOP(Polygon,
		MOVE(point->X, point->Y, DeltaX, DeltaY);
	);
	MOVE_BOX_LOWLEVEL(&Polygon->BoundingBox, DeltaX, DeltaY);
}

/* ---------------------------------------------------------------------------
 * moves a polygon
 */
static void *MovePolygon(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	if (Layer->On)
	{
		ErasePolygon(Polygon);
		MovePolygonLowLevel(Polygon, DeltaX, DeltaY);
		DrawPolygon(Layer, Polygon);
	}
	else
		MovePolygonLowLevel(Polygon, DeltaX, DeltaY);
	return(Polygon);
}

/* ---------------------------------------------------------------------------
 * moves one end of a line
 */
static void *MoveLinePoint(LayerTypePtr Layer, LineTypePtr Line,
	PointTypePtr Point)
{
	if (Layer->On)
	{
		EraseLine(Line);
		MOVE(Point->X, Point->Y, DeltaX, DeltaY)
		DrawLine(Layer, Line);
	}
	else
		MOVE(Point->X, Point->Y, DeltaX, DeltaY)
	return(Line);
}

/* ---------------------------------------------------------------------------
 * moves a polygon-point
 */
static void *MovePolygonPoint(LayerTypePtr Layer, PolygonTypePtr Polygon,
	PointTypePtr Point)
{
	if (Layer->On)
	{
		ErasePolygon(Polygon);
		MOVE(Point->X, Point->Y, DeltaX, DeltaY);
		DrawPolygon(Layer, Polygon);
	}
	else
		MOVE(Point->X, Point->Y, DeltaX, DeltaY);
	SetPolygonBoundingBox(Polygon);
	return(Point);
}

/* ---------------------------------------------------------------------------
 * moves a line between layers; lowlevel routines
 */
void *MoveLineToLayerLowLevel(LayerTypePtr Source, LineTypePtr Line,
	LayerTypePtr Destination)
{
	LineTypePtr		new = GetLineMemory(Destination);

		/* copy the data and remove it from the former layer */
	*new = *Line;
	*Line = Source->Line[--Source->LineN];
	memset(&Source->Line[Source->LineN], 0, sizeof(LineType));
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves a line between layers
 */
static void *MoveLineToLayer(LayerTypePtr Layer, LineTypePtr Line)
{
	LineTypePtr	new;

	AddObjectToMoveToLayerUndoList(LINE_TYPE, Layer, Line, Line);
	if (Layer->On)
		EraseLine(Line);
	new = MoveLineToLayerLowLevel(Layer, Line, Dest);
	if (Dest->On)
		DrawLine(Dest, new);
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves a text object between layers; lowlevel routines
 */
void *MoveTextToLayerLowLevel(LayerTypePtr Source, TextTypePtr Text,
	LayerTypePtr Destination)
{
	TextTypePtr		new = GetTextMemory(Destination);

		/* copy the data and remove it from the former layer */
	*new = *Text;
	*Text = Source->Text[--Source->TextN];
	memset(&Source->Text[Source->TextN], 0, sizeof(TextType));
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves a text object between layers
 */
static void *MoveTextToLayer(LayerTypePtr Layer, TextTypePtr Text)
{
	TextTypePtr	new;

	AddObjectToMoveToLayerUndoList(TEXT_TYPE, Layer, Text, Text);
	if (Layer->On)
		EraseText(Text);
	new = MoveTextToLayerLowLevel(Layer, Text, Dest);
	if (Dest->On)
		DrawText(Dest, new);
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves a polygon between layers; lowlevel routines
 */
void *MovePolygonToLayerLowLevel(LayerTypePtr Source, PolygonTypePtr Polygon,
	LayerTypePtr Destination)
{
	PolygonTypePtr		new = GetPolygonMemory(Destination);

		/* copy the data and remove it from the former layer */
	*new = *Polygon;
	*Polygon = Source->Polygon[--Source->PolygonN];
	memset(&Source->Polygon[Source->PolygonN], 0, sizeof(PolygonType));
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves a polygon between layers
 */
static void *MovePolygonToLayer(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	PolygonTypePtr	new;

	AddObjectToMoveToLayerUndoList(POLYGON_TYPE, Layer, Polygon, Polygon);
	if (Layer->On)
		ErasePolygon(Polygon);
	new = MovePolygonToLayerLowLevel(Layer, Polygon, Dest);
	if (Dest->On)
		DrawPolygon(Dest, new);
	return(new);
}

/* ---------------------------------------------------------------------------
 * moves the object identified by its data pointers and the type
 */
void *MoveObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position DX, Position DY)
{
		/* setup offset */
	DeltaX = DX;
	DeltaY = DY;
	AddObjectToMoveUndoList(Type, Ptr1, Ptr2, Ptr3, DX, DY);
	IncrementUndoSerialNumber();
	return(ObjectOperation(&MoveFunctions, Type, Ptr1, Ptr2, Ptr3));
}

/* ---------------------------------------------------------------------------
 * moves the object identified by its data pointers and the type
 * as well as all attached rubberband lines
 */
void *MoveObjectAndRubberband(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position DX, Position DY)
{
	RubberbandTypePtr	ptr;

		/* setup offset */
	DeltaX = DX;
	DeltaY = DY;

		/* move all the lines... and reset the counter */
	ptr = Crosshair.AttachedObject.Rubberband;
	while (Crosshair.AttachedObject.RubberbandN)
	{
		AddObjectToMoveUndoList(LINEPOINT_TYPE, 
			ptr->Layer, ptr->Line, ptr->MovedPoint, DX, DY);
		MoveLinePoint(ptr->Layer, ptr->Line, ptr->MovedPoint);
		Crosshair.AttachedObject.RubberbandN--;
		ptr++;
	}

	AddObjectToMoveUndoList(Type, Ptr1, Ptr2, Ptr3, DX, DY);
	IncrementUndoSerialNumber();
	return(ObjectOperation(&MoveFunctions, Type, Ptr1, Ptr2, Ptr3));
}

/* ---------------------------------------------------------------------------
 * moves the object identified by its data pointers and the type
 * to a new layer without changing it's position
 */
void *MoveObjectToLayer(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	LayerTypePtr Target)
{
	void	*result;

		/* setup global identifiers */
	Dest = Target;
	result = ObjectOperation(&MoveToLayerFunctions, Type, Ptr1, Ptr2, Ptr3);
	IncrementUndoSerialNumber();
	return(result);
}

/* ---------------------------------------------------------------------------
 * moves the selected objects to a new layer without changing their
 * positions
 */
Boolean MoveSelectedObjectsToLayer(LayerTypePtr Target)
{
	Boolean	changed;

		/* setup global identifiers */
	Dest = Target;
	changed = SelectedOperation(&MoveToLayerFunctions, True);
	IncrementUndoSerialNumber();
	return(changed);
}

