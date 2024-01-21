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

static	char	*rcsid = "$Id: create.c,v 143.1 1996/09/16 09:08:31 nau Exp $";

/* functions used to create vias, pins ...
 */

#include <stdlib.h>
#include <memory.h>

#include "global.h"

#include "create.h"
#include "data.h"
#include "dialog.h"
#include "draw.h"
#include "error.h"
#include "mymem.h"
#include "misc.h"
#include "parse_l.h"
#include "set.h"

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	int		ID = 1;			/* current object ID; incremented after */
								/* each creation of an object */

/* ----------------------------------------------------------------------
 * some local prototypes
 */
static	void	AddTextToElement(TextTypePtr, FontTypePtr,
					Position, Position, BYTE, char *, int, int);

/* ---------------------------------------------------------------------------
 * creates a new paste buffer
 */
DataTypePtr CreateNewBuffer(void)
{
	return((DataTypePtr) MyCalloc(1, sizeof(DataType), "CreateNewBuffer()"));
}

/* ---------------------------------------------------------------------------
 * creates a new PCB
 */
PCBTypePtr CreateNewPCB(Boolean SetDefaultNames)
{
	PCBTypePtr	ptr;
	int			i;

		/* allocate memory, switch all layers on and copy resources */
	ptr = MyCalloc(1, sizeof(PCBType), "CreateNewPCB()");
	ptr->Data = CreateNewBuffer();

		/* copy default settings */
	ptr->ConnectedColor = Settings.ConnectedColor;
	ptr->ElementColor = Settings.ElementColor;
	ptr->InvisibleObjectsColor = Settings.InvisibleObjectsColor;
	ptr->ElementSelectedColor = Settings.ElementSelectedColor;
	ptr->PinColor = Settings.PinColor;
	ptr->PinSelectedColor = Settings.PinSelectedColor;
	ptr->ViaColor = Settings.ViaColor;
	ptr->ViaSelectedColor = Settings.ViaSelectedColor;
	ptr->GroundplaneColor = Settings.GroundplaneColor;
	for (i = 0; i < MAX_LAYER; i++)
	{
		ptr->Data->Layer[i].Color = Settings.LayerColor[i];
		ptr->Data->Layer[i].SelectedColor = Settings.LayerSelectedColor[i];
	}
	if (SetDefaultNames)
		for (i = 0; i < MAX_LAYER; i++)
			ptr->Data->Layer[i].Name = MyStrdup(Settings.DefaultLayerName[i],
				"CreateNewPCB()");

	if (Settings.AbsoluteGrid)
		SET_FLAG(ABSOLUTEFLAG, ptr);
	if (Settings.AllDirectionLines)
		SET_FLAG(ALLDIRCETIONFLAG, ptr);
	ptr->Grid = Settings.Grid;
	ptr->LayerGroups = Settings.LayerGroups;
	ptr->Zoom = Settings.Zoom;
	ptr->MaxWidth = Settings.MaxWidth;
	ptr->MaxHeight = Settings.MaxHeight;

		/* no pixmap allocated yet */
	ptr->SaveUnder = BadAlloc;
	ptr->ID = ID++;
	return(ptr);
}

/* ---------------------------------------------------------------------------
 * creates a new via
 */
PinTypePtr CreateNewVia(DataTypePtr Data,
	Position X, Position Y,
	Dimension Thickness, Dimension DrillingHole,
	char *Name, int Flags)
{
	PinTypePtr via = GetViaMemory(Data);

		/* copy values */
	via->X = X;
	via->Y = Y;
	via->Thickness = Thickness;
	via->DrillingHole = DrillingHole;
	via->Name = MyStrdup(Name, "CreateNewVia()");
	via->Flags = Flags;
	via->ID = ID++;
	return(via);
}

/* ---------------------------------------------------------------------------
 * creates a new line on a layer
 */
LineTypePtr CreateNewLineOnLayer(LayerTypePtr Layer,
	Position X1, Position Y1,
	Position X2, Position Y2,
	Dimension Thickness, int Flags)
{
	LineTypePtr	line = GetLineMemory(Layer);

	line->ID = ID++;
	line->Flags = Flags;
	line->Thickness = Thickness;
	line->Point1.X = X1;
	line->Point1.Y = Y1;
	line->Point1.ID = ID++;
	line->Point2.X = X2;
	line->Point2.Y = Y2;
	line->Point2.ID = ID++;
	return(line);
}

/* ---------------------------------------------------------------------------
 * creates a new polygon from the old formats rectangle data
 */
PolygonTypePtr CreateNewPolygonFromRectangle(LayerTypePtr Layer,
	Position X1, Position Y1,
	Position X2, Position Y2,
	int Flags)
{
	PolygonTypePtr	polygon = CreateNewPolygon(Layer, Flags);

	CreateNewPointInPolygon(polygon, X1, Y1);
	CreateNewPointInPolygon(polygon, X2, Y1);
	CreateNewPointInPolygon(polygon, X2, Y2);
	CreateNewPointInPolygon(polygon, X1, Y2);
	SetPolygonBoundingBox(polygon);
	return(polygon);
}

/* ---------------------------------------------------------------------------
 * creates a new text on a layer
 */
TextTypePtr CreateNewText(LayerTypePtr Layer, FontTypePtr PCBFont,
	Position X, Position Y,
	BYTE Direction, int Scale, char *TextString, int Flags)
{
	TextTypePtr	text = GetTextMemory(Layer);

		/* copy values, width and height are set by drawing routine
		 * because at ths point we don't know which symbols are available
		 */
	text->X = X;
	text->Y = Y;
	text->Direction = Direction;
	text->Flags = Flags;
	text->Scale = Scale;
	text->TextString = MyStrdup(TextString, "CreateNewText()");

		/* calculate size of the bounding box */
	SetTextBoundingBox(PCBFont, text);
	text->ID = ID++;
	return(text);
}

/* ---------------------------------------------------------------------------
 * creates a new polygon on a layer
 */
PolygonTypePtr CreateNewPolygon(LayerTypePtr Layer, int Flags)
{
	PolygonTypePtr	polygon = GetPolygonMemory(Layer);

		/* copy values */
	polygon->Flags = Flags;
	polygon->ID = ID++;
	return(polygon);
}

/* ---------------------------------------------------------------------------
 * creates a new point in a polygon
 */
PointTypePtr CreateNewPointInPolygon(PolygonTypePtr Polygon,
	Position X, Position Y)
{
	PointTypePtr	point = GetPointMemoryInPolygon(Polygon);

		/* copy values */
	point->X = X;
	point->Y = Y;
	point->ID = ID++;
	return(point);
}

/* ---------------------------------------------------------------------------
 * creates an new element
 * memory is allocated if needed
 */
ElementTypePtr CreateNewElement(DataTypePtr Data, ElementTypePtr Element,
	FontTypePtr PCBFont,
	int Flags,
	char *Description, char *NameOnPCB, char *Value,
	Position TextX, Position TextY, BYTE Direction,
	int TextScale, int TextFlags)
{
	if (!Element)
		Element = GetElementMemory(Data);

		/* copy values and set additional information */
	AddTextToElement(&DESCRIPTION_TEXT(Element), PCBFont, TextX, TextY,
		Direction, Description, TextScale, TextFlags);
	AddTextToElement(&NAMEONPCB_TEXT(Element), PCBFont, TextX, TextY,
		Direction, NameOnPCB, TextScale, TextFlags);
	AddTextToElement(&VALUE_TEXT(Element), PCBFont, TextX, TextY,
		Direction, Value, TextScale, TextFlags);
	Element->Flags = Flags;
	Element->ID = ID++;
	return(Element);
}

/* ---------------------------------------------------------------------------
 * creates a new arc in an element
 */
ArcTypePtr CreateNewArcInElement(ElementTypePtr Element,
	Position X, Position Y,
	Dimension Width, Dimension Height,
	int Angle, int Delta,
	Dimension Thickness)
{
	ArcTypePtr	arc = Element->Arc;

		/* realloc new memory if necessary and clear it */
	if (Element->ArcN >= Element->ArcMax)
	{
		Element->ArcMax += STEP_ELEMENTARC;	
		arc = MyRealloc(arc, Element->ArcMax *sizeof(ArcType),
			"CreateNewArcInElement()");
		Element->Arc = arc;
		memset(arc +Element->ArcN, 0, STEP_ELEMENTARC*sizeof(ArcType));
	}

		/* set Delta (0,360], StartAngle in [0,360) */
	if ((Delta = Delta % 360) == 0)
		Delta = 360;
	if (Delta < 0)
	{
		Angle += Delta;
		Delta = -Delta;
	}
	if ((Angle = Angle % 360) < 0)
		Angle += 360;

		/* copy values */
	arc = arc +Element->ArcN++;
	arc->X = X;
	arc->Y = Y;
	arc->Width = Width;
	arc->Height = Height;
	arc->StartAngle = Angle;
	arc->Delta = Delta;
	arc->Thickness = Thickness;
	return(arc);
}

/* ---------------------------------------------------------------------------
 * creates a new arc for an element
 */
LineTypePtr CreateNewLineInElement(ElementTypePtr Element,
	Position X1, Position Y1,
	Position X2, Position Y2,
	Dimension Thickness)
{
	LineTypePtr	line = Element->Line;

		/* realloc new memory if necessary and clear it */
	if (Element->LineN >= Element->LineMax)
	{
		Element->LineMax += STEP_ELEMENTLINE;	
		line = MyRealloc(line, Element->LineMax *sizeof(LineType),
			"CreateNewLineInElement()");
		Element->Line = line;
		memset(line +Element->LineN, 0, STEP_ELEMENTLINE*sizeof(LineType));
	}

		/* copy values */
	line = line +Element->LineN++;
	line->Point1.X = X1;
	line->Point1.Y = Y1;
	line->Point2.X = X2;
	line->Point2.Y = Y2;
	line->Thickness = Thickness;
	return(line);
}

/* ---------------------------------------------------------------------------
 * creates a new pin in an element
 */
PinTypePtr CreateNewPin(ElementTypePtr Element,
	Position X, Position Y,
	Dimension Thickness, Dimension DrillingHole,
	char *Name, int Flags)
{
	PinTypePtr pin = GetPinMemory(Element);

		/* copy values */
	pin->X = X;
	pin->Y = Y;
	pin->Thickness = Thickness;
	pin->DrillingHole = DrillingHole;
	pin->Name = MyStrdup(Name, "CreateNewPin()");
	pin->Flags = Flags;
	pin->ID = ID++;
	return(pin);
}

/* ---------------------------------------------------------------------------
 * creates a new pad in an element
 */
PadTypePtr CreateNewPad(ElementTypePtr Element,
	Position X1, Position Y1, Position X2, Position Y2,
	Dimension Thickness, char *Name, int Flags)
{
	PadTypePtr pad = GetPadMemory(Element);

		/* copy values */
	pad->Point1.X = X1;
	pad->Point1.Y = Y1;
	pad->Point2.X = X2;
	pad->Point2.Y = Y2;
	pad->Thickness = Thickness;
	pad->Name = MyStrdup(Name, "CreateNewPad()");
	pad->Flags = Flags;
	pad->ID = ID++;
	return(pad);
}

/* ---------------------------------------------------------------------------
 * creates a new textobject as part of an element
 * copies the values to the appropriate text object
 */
static void AddTextToElement(TextTypePtr Text, FontTypePtr PCBFont,
	Position X, Position Y,
	BYTE Direction, char *TextString, int Scale, int Flags)
{
	MyFree(&Text->TextString);
	Text->X = X;
	Text->Y = Y;
	Text->Direction = Direction;
	Text->Flags = Flags;
	Text->Scale = Scale;
	Text->TextString = (TextString && *TextString) ?
		MyStrdup(TextString, "AddTextToElement()") : NULL;

		/* calculate size of the bounding box */
	SetTextBoundingBox(PCBFont, Text);
	Text->ID = ID++;
}

/* ---------------------------------------------------------------------------
 * creates a new line in a symbol
 */
LineTypePtr CreateNewLineInSymbol(SymbolTypePtr Symbol,
	Position X1, Position Y1,
	Position X2, Position Y2,
	Dimension Thickness)
{
	LineTypePtr	line = Symbol->Line;

		/* realloc new memory if necessary and clear it */
	if (Symbol->LineN >= Symbol->LineMax)
	{
		Symbol->LineMax += STEP_SYMBOLLINE;	
		line = MyRealloc(line, Symbol->LineMax *sizeof(LineType),
			"CreateNewLineInSymbol()");
		Symbol->Line = line;
		memset(line +Symbol->LineN, 0, STEP_SYMBOLLINE*sizeof(LineType));
	}

		/* copy values */
	line = line +Symbol->LineN++;
	line->Point1.X = X1;
	line->Point1.Y = Y1;
	line->Point2.X = X2;
	line->Point2.Y = Y2;
	line->Thickness = Thickness;
	return(line);
}

/* ---------------------------------------------------------------------------
 * parses a file with font information and installs it
 * checks directories given as colon seperated list by resource fontPath
 * if the fonts filename doesn't contain a directory component
 */
void CreateDefaultFont(void)
{
	if (ParseFont(&PCB->Font, Settings.FontFile))
		Message("can't find font-symbol-file '%s'\n", Settings.FontFile);
}

/* ---------------------------------------------------------------------------
 * adds a new line to the rubberband list of 'Crosshair.AttachedObject'
 */
RubberbandTypePtr CreateNewRubberbandEntry(LayerTypePtr Layer,
	LineTypePtr Line, PointTypePtr MovedPoint)
{
	RubberbandTypePtr	ptr = GetRubberbandMemory();

	ptr->Layer = Layer;
	ptr->Line = Line;
	ptr->MovedPoint = MovedPoint;
	return(ptr);
}
