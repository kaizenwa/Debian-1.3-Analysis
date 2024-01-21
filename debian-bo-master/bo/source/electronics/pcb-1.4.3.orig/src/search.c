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

static	char	*rcsid = "$Id: search.c,v 143.1 1996/09/16 09:08:57 nau Exp $";

/* search routines
 * some of the functions use dummy parameters
 */
#include <math.h>

#include "global.h"

#include "data.h"
#include "draw.h"
#include "find.h"
#include "search.h"

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Position		PosX,		/* search position for subroutines */
						PosY;

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	Boolean	SearchLineByPosition(LayerTypePtr *, LineTypePtr *,
					LineTypePtr *);
static	Boolean	SearchTextByPosition(LayerTypePtr *, TextTypePtr *,
					TextTypePtr *);
static	Boolean	SearchPolygonByPosition(LayerTypePtr *, PolygonTypePtr *,
					PolygonTypePtr *);
static	Boolean	SearchPinByPosition(ElementTypePtr *, PinTypePtr *,
					PinTypePtr *);
static	Boolean	SearchPadByPosition(ElementTypePtr *, PadTypePtr *,
					PadTypePtr *);
static	Boolean	SearchViaByPosition(PinTypePtr *, PinTypePtr *, PinTypePtr *);
static	Boolean	SearchElementNameByPosition(ElementTypePtr *, TextTypePtr *,
					TextTypePtr *);
static	Boolean	SearchLinePointByPosition(LayerTypePtr *, LineTypePtr *,
					PointTypePtr *);
static	Boolean	SearchPointByPosition(LayerTypePtr *, PolygonTypePtr *,
					PointTypePtr *);


/* ---------------------------------------------------------------------------
 * searches a via
 */
static Boolean SearchViaByPosition(PinTypePtr *Via, PinTypePtr *Dummy1,
	PinTypePtr *Dummy2)
{
		/* search only if via-layer is visible */
	if (PCB->ViaOn)
		VIA_LOOP(PCB->Data,
			if (abs(via->X - PosX) <= via->Thickness/3 &&
		    	abs(via->Y - PosY) <= via->Thickness/3 )
			{
				*Via = *Dummy1 = *Dummy2 = via;
				return(True);
			}
		);
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches a pin
 * starts with the newest element
 */
static Boolean SearchPinByPosition(ElementTypePtr *Element, PinTypePtr *Pin,
	PinTypePtr *Dummy)
{
		/* search only if pin-layer is visible */
	if (PCB->PinOn)
		ELEMENT_LOOP(PCB->Data,
			PIN_LOOP(element,
				if (abs(pin->X - PosX) <= pin->Thickness/3 &&
					abs(pin->Y - PosY) <= pin->Thickness/3 )
				{
						/* bug-fix by Ulrich Pegelow (ulrpeg@bigcomm.gun.de) */
					*Element = element;
					*Pin = *Dummy = pin;
					return(True);
				}
			);
		);
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches a pad
 * starts with the newest element
 */
static Boolean SearchPadByPosition(ElementTypePtr *Element, PadTypePtr *Pad,
	PadTypePtr *Dummy)
{
		/* search only if pin-layer is visible */
	if (PCB->PinOn)
		ALLPAD_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
			{
					/* the cast isn't very nice but working, check
					 * global.h for details
					 */
				if (IsPointOnLine(PosX, PosY, 0, (LineTypePtr) pad))
				{
					*Pad = *Dummy = pad;
					return(True);
				}
			}
		);
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches line on all layers that are switched on in layerstack order
 */
static Boolean SearchLineByPosition(LayerTypePtr *Layer, LineTypePtr *Line,
	LineTypePtr *Dummy)
{
	Cardinal		i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		*Layer = LAYER_ON_STACK(i);
		if ((*Layer)->On)
			LINE_LOOP(*Layer,
				if (IsPointOnLine(PosX, PosY, 0, line))
				{
					*Line = *Dummy = line;
					return(True);
				}
			);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches text on all layers that are switched on in layerstack order
 */
static Boolean SearchTextByPosition(LayerTypePtr *Layer, TextTypePtr *Text,
	TextTypePtr *Dummy)
{
	Cardinal		i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		*Layer = LAYER_ON_STACK(i);
		if ((*Layer)->On)
			TEXT_LOOP(*Layer,
				if (POINT_IN_BOX(PosX, PosY, &text->BoundingBox))
				{
					*Text = *Dummy = text;
					return(True);
				}
			);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches a polygon on all layers that are switched on in layerstack order
 */
static Boolean SearchPolygonByPosition(LayerTypePtr *Layer,
	PolygonTypePtr *Polygon, PolygonTypePtr *Dummy)
{
	Cardinal		i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		*Layer = LAYER_ON_STACK(i);
		if ((*Layer)->On)
			POLYGON_LOOP(*Layer,
				if (IsPointInPolygon(PosX, PosY, 0, polygon))
				{
					*Polygon = *Dummy = polygon;
					return(True);
				}
			);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches a line-point on all layers that are switched on
 * in layerstack order
 *
 */
static Boolean SearchLinePointByPosition(LayerTypePtr *Layer, LineTypePtr *Line,
	PointTypePtr *Point)
{
	Cardinal		i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		float			d;

		*Layer = LAYER_ON_STACK(i);
		if ((*Layer)->On)
			LINE_LOOP(*Layer,
			{
					/* some stupid code to check both points */
				d = (PosX - line->Point1.X)*(PosX - line->Point1.X)+
					(PosY - line->Point1.Y)*(PosY - line->Point1.Y);
				if (d < MAX_LINE_POINT_DISTANCE*MAX_LINE_POINT_DISTANCE)
				{
					*Line = line;
					*Point = &line->Point1;
					return(True);
				}

				d = (PosX - line->Point2.X)*(PosX - line->Point2.X)+
					(PosY - line->Point2.Y)*(PosY - line->Point2.Y);
				if (d < MAX_LINE_POINT_DISTANCE*MAX_LINE_POINT_DISTANCE)
				{
					*Line = line;
					*Point = &line->Point2;
					return(True);
				}
			}
			);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches a polygon-point on all layers that are switched on
 * in layerstack order
 */
static Boolean SearchPointByPosition(LayerTypePtr *Layer,
	PolygonTypePtr *Polygon, PointTypePtr *Point)
{
	Cardinal	i;

	for (i = 0; i < MAX_LAYER; i++)
	{
		float			d;

		*Layer = LAYER_ON_STACK(i);
		if ((*Layer)->On)
			POLYGON_LOOP(*Layer,
				POLYGONPOINT_LOOP(polygon,
				{
					d = (point->X - PosX)*(point->X - PosX)+
						(point->Y - PosY)*(point->Y - PosY);
					if (d < MAX_POLYGON_POINT_DISTANCE*MAX_POLYGON_POINT_DISTANCE)
					{
						*Polygon = polygon;
						*Point = point;
						return(True);
					}
				}
				);
			);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches the name of an element
 * the search starts with the last element and goes back to the beginning
 */
static Boolean SearchElementNameByPosition(ElementTypePtr *Element,
	TextTypePtr *Text, TextTypePtr *Dummy)
{
	TextTypePtr		text;

		/* package layer have to be switched on */
	if (PCB->ElementOn)
	{
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
			{
				text = &ELEMENT_TEXT(PCB, element);
				if (POINT_IN_BOX(PosX, PosY, &text->BoundingBox))
				{
					*Element = element;
					*Text = *Dummy = text;
					return(True);
				}
			}
		);
	}
	return(False);
}

/* ---------------------------------------------------------------------------
 * searches an element
 * the search starts with the last element and goes back to the beginning
 * if more than one element matches, the smallest one is taken
 */
static Boolean SearchElementByPosition(ElementTypePtr *Element,
	ElementTypePtr *Dummy1, ElementTypePtr *Dummy2)
{
	ElementTypePtr	save = NULL;
	long	 		area = 0;
	Boolean			found;

		/* package layer have to be switched on */
	if (PCB->ElementOn || PCB->PinOn)
	{
			/* the element names bounding box is not necessarily
			 * a part of the elements bounding box;
			 * we have to check all of them
			 */
		ELEMENT_LOOP(PCB->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
				PCB->InvisibleObjectsOn)
			{
				found = POINT_IN_BOX(PosX, PosY, &element->BoundingBox);
				ELEMENTTEXT_LOOP(element,
					found |= POINT_IN_BOX(PosX, PosY, &text->BoundingBox);
				);
				if (found)
				{
					long	newarea;

						/* use the element with the smallest bounding box */
					newarea=(element->BoundingBox.X2 -element->BoundingBox.X1) *
						(element->BoundingBox.Y2 -element->BoundingBox.Y1);
					if (!save || newarea < area)
					{
						area = newarea;
						save = element;
					}
				}
			}
		);
	}
	*Element = *Dummy1 = *Dummy2 = save;
	return(save != NULL);
}

/* ---------------------------------------------------------------------------
 * checks if a line intersects with a PV
 * constant recognition by the optimizer is assumed
 *
 * let the point be (X,Y) and the line (X1,Y1)(X2,Y2)
 * the length of the line is
 *
 *   l = ((X2-X1)^2 + (Y2-Y1)^2)^0.5
 * 
 * let Q be the point of perpendicular projection of (X,Y) onto the line
 *
 *   QX = X1 +r*(X2-X1)
 *   QY = Y1 +r*(Y2-Y1)
 * 
 * with (from vector geometry)
 *
 *       (Y1-Y)(Y1-Y2)+(X1-X)(X1-X2)
 *   r = ---------------------------
 *                   l*l
 *
 *   r < 0     Q is on backward extension of the line
 *   r > 1     Q is on forward extension of the line
 *   else      Q is on the line
 *
 * the signed distance from (X,Y) to Q is
 *
 *       (Y2-Y1)(X-X1)-(X2-X1)(Y-Y1)
 *   d = ----------------------------
 *                    l
 */
Boolean IsPointOnLine(float X, float Y, float Radius, LineTypePtr Line)
{
	register	float	dx, dy,
						dx1, dy1,
						l,
						d,
						r;

	dx = (float) (Line->Point2.X -Line->Point1.X);
	dy = (float) (Line->Point2.Y -Line->Point1.Y);
	dx1 = (float) (Line->Point1.X -X);
	dy1 = (float) (Line->Point1.Y -Y);
	if ((l = dx*dx +dy*dy) == 0.0)
		return(False);

	d = dx*dy1 -dy*dx1;

		/* check distance from PV to line */
	Radius += ((float) Line->Thickness)/2.0;
	Radius *= Radius;
	if (d*d > Radius*l)
		return(False);

		/* they intersect if Q is on line */
	r = -(dx*dx1 +dy*dy1);
	if (r >= 0 && r <= l)
		return(True);

		/* we have to check P1 or P2 depending on the sign of r */
	if (r < 0.0)
		return((dx1*dx1 +dy1*dy1) <= Radius);
	dx1 = Line->Point2.X -X;
	dy1 = Line->Point2.Y -Y;
	return((dx1*dx1 +dy1*dy1) <= Radius);
}

/* ---------------------------------------------------------------------------
 * checks if a line crosses a square
 */
Boolean IsLineInRectangle(Position X1, Position Y1,
	Position X2, Position Y2, LineTypePtr Line)
{
	LineType	line;
	Dimension	thick = Line->Thickness/2 +1;
	Position	minx = MIN(Line->Point1.X, Line->Point2.X) -thick,
				miny = MIN(Line->Point1.Y, Line->Point2.Y) -thick,
				maxx = MAX(Line->Point1.X, Line->Point2.X) +thick,
				maxy = MAX(Line->Point1.Y, Line->Point2.Y) +thick;

		/* construct a set of dummy lines and check each of them */
	line.Thickness = 0;

		/* upper-left to upper-right corner */
	line.Point1.Y = line.Point2.Y = Y1;
	if (miny <= line.Point1.Y && maxy >= line.Point1.Y)
	{
		line.Point1.X = X1;
		line.Point2.X = X2;
		if (LineLineIntersect(&line, Line))
			return(True);
	}

		/* upper-right to lower-right corner */
	line.Point1.X = line.Point2.X = X2;
	if (minx <= line.Point1.X && maxx >= line.Point1.X)
	{
		line.Point1.Y = Y1;
		line.Point2.Y = Y2;
		if (LineLineIntersect(&line, Line))
			return(True);
	}

		/* lower-right to lower-left corner */
	line.Point1.Y = line.Point2.Y = Y2;
	if (miny <= line.Point1.Y && maxy >= line.Point1.Y)
	{
		line.Point1.X = X1;
		line.Point2.X = X2;
		if (LineLineIntersect(&line, Line))
			return(True);
	}

		/* lower-left to upper-left corner */
	line.Point1.X = line.Point2.X = X1;
	if (minx <= line.Point1.X && maxx >= line.Point1.X)
	{
		line.Point1.Y = Y1;
		line.Point2.Y = Y2;
		if (LineLineIntersect(&line, Line))
			return(True);
	}

	return(False);
}

/* ---------------------------------------------------------------------------
 * checks if a point lies inside a polygon
 * the code assumes that the last point isn't equal to the first one
 * The algorithm fails if the point is equal to a corner
 * from news FAQ:
 *   This code is from Wm. Randolph Franklin, wrf@ecse.rpi.edu, a
 *   professor at RPI.
 *
 * extentions:
 * check if the distance between the polygon lines and the point is less
 * or equal to the radius.
 */
Boolean IsPointInPolygon(float X, float Y, float Radius, PolygonTypePtr Polygon)
{
	Boolean			inside = False;
	BoxType			boundingbox = Polygon->BoundingBox;

		/* increment the size of the bounding box by the radius */
	boundingbox.X1 -= (Position) Radius;
	boundingbox.Y1 -= (Position) Radius;
	boundingbox.X2 += (Position) Radius;
	boundingbox.Y2 += (Position) Radius;

		/* quick check if the point may lay inside */
	if (POINT_IN_BOX(X, Y, &boundingbox))
	{
		PointTypePtr	ptr = &Polygon->Points[0];

			/* POLYGONPOINT_LOOP decrements pointers !!! */
		POLYGONPOINT_LOOP(Polygon,
			if ((((ptr->Y <= Y) && (Y < point->Y)) ||
				((point->Y <= Y) && (Y < ptr->Y))) &&
				(X < ((float) (point->X -ptr->X) * (float) (Y -ptr->Y) /
					(float) (point->Y -ptr->Y) +ptr->X)))
				inside = !inside;
			ptr = point;
		);

			/* check the distance between the lines of the
			 * polygon and the point itself
			 *
			 * check is done by contructing a dummy line
			 */
		if (!inside)
		{
			LineType	line;

			line.Point1 = Polygon->Points[0];
			line.Thickness = 0;

				/* POLYGONPOINT_LOOP decrements pointers !!! */
			POLYGONPOINT_LOOP(Polygon,
				line.Point2 = *point;
				if (IsPointOnLine(X, Y, Radius, &line))
					return(True);
				line.Point1 = *point;
			);
			
		}
	}
	return(inside);
}

/* ---------------------------------------------------------------------------
 * checks is a polygon intersects with a square
 */
Boolean IsRectangleInPolygon(Position X1, Position Y1,
	Position X2, Position Y2, PolygonTypePtr Polygon)
{
	PolygonType	polygon;
	PointType	points[4];

		/* construct a dummy polygon and check it */
	polygon.BoundingBox.X1 = points[0].X = points[3].X = X1;
	polygon.BoundingBox.X2 = points[1].X = points[2].X = X2;
	polygon.BoundingBox.Y1 = points[0].Y = points[1].Y = Y1;
	polygon.BoundingBox.Y2 = points[2].Y = points[3].Y = Y2;
	polygon.Points = points;
	polygon.PointN = 4;
	return(IsPolygonInPolygon(&polygon, Polygon));
}

/* ---------------------------------------------------------------------------
 * searches for any kind of object or for a set of object types
 * the calling routine passes two pointers to allocated memory for storing
 * the results. 
 * A type value is returned too which is NO_TYPE if no objects has been found.
 * A set of object types is passed in.
 * The object is located by it's position.
 *
 * The layout is checked in the following order:
 *   polygon-point, pin, via, line, text, elementname, polygon, element
 */
int SearchObjectByPosition(int Type,
	void **Result1, void **Result2, void **Result3,
	Position X, Position Y)
{
		/* setup local identifiers */
	PosX = X;
	PosY = Y;

	if (Type & LINEPOINT_TYPE &&
		SearchLinePointByPosition((LayerTypePtr *) Result1,
			(LineTypePtr *) Result2,
			(PointTypePtr *) Result3))
		return(LINEPOINT_TYPE);
	
	if (Type & POLYGONPOINT_TYPE &&
		SearchPointByPosition((LayerTypePtr *) Result1,
			(PolygonTypePtr *) Result2,
			(PointTypePtr *) Result3))
		return(POLYGONPOINT_TYPE);

	if (Type & PIN_TYPE &&
		SearchPinByPosition((ElementTypePtr *) Result1,
			(PinTypePtr *) Result2, (PinTypePtr *) Result3))
		return(PIN_TYPE);
	
	if (Type & PAD_TYPE &&
		SearchPadByPosition((ElementTypePtr *) Result1,
			(PadTypePtr *) Result2, (PadTypePtr *) Result3))
		return(PAD_TYPE);
	
	if (Type & VIA_TYPE &&
		SearchViaByPosition((PinTypePtr *) Result1,
			(PinTypePtr *) Result2, (PinTypePtr *) Result3))
		return(VIA_TYPE);

	if (Type & LINE_TYPE &&
		SearchLineByPosition((LayerTypePtr *) Result1,
			(LineTypePtr *) Result2, (LineTypePtr *) Result3))
		return(LINE_TYPE);

	if (Type & TEXT_TYPE &&
		SearchTextByPosition((LayerTypePtr *) Result1,
			(TextTypePtr *) Result2, (TextTypePtr *) Result3))
		return(TEXT_TYPE);

	if (Type & ELEMENTNAME_TYPE &&
		SearchElementNameByPosition((ElementTypePtr *) Result1,
			(TextTypePtr *) Result2, (TextTypePtr *) Result3))
		return(ELEMENTNAME_TYPE);

	if (Type & POLYGON_TYPE &&
		SearchPolygonByPosition((LayerTypePtr *) Result1,
			(PolygonTypePtr *) Result2, (PolygonTypePtr *) Result3))
		return(POLYGON_TYPE);

	if (Type & ELEMENT_TYPE &&
		SearchElementByPosition((ElementTypePtr *) Result1,
			(ElementTypePtr *) Result2, (ElementTypePtr *) Result3))
		return(ELEMENT_TYPE);

	return(NO_TYPE);
}

/* ---------------------------------------------------------------------------
 * searches for a object by it's unique ID. It doesn't matter if
 * the object is visible or not. The search is performed on a PCB, a
 * buffer or on the remove list.
 * The calling routine passes two pointers to allocated memory for storing
 * the results. 
 * A type value is returned too which is NO_TYPE if no objects has been found.
 */
int SearchObjectByID(DataTypePtr Base,
	void **Result1, void **Result2, void **Result3, int ID)
{
	ALLLINE_LOOP(Base,
		if (line->ID == ID)
		{
			*Result1 = (void *) layer;
			*Result2 = *Result3 = (void *) line;
			return(LINE_TYPE);
		}
		if (line->Point1.ID == ID)
		{
			*Result1 = (void *) layer;
			*Result2 = (void *) line;
			*Result3 = (void *) &line->Point1;
			return(LINEPOINT_TYPE);
		}
		if (line->Point2.ID == ID)
		{
			*Result1 = (void *) layer;
			*Result2 = (void *) line;
			*Result3 = (void *) &line->Point2;
			return(LINEPOINT_TYPE);
		}
	);

	ALLTEXT_LOOP(Base,
		if (text->ID == ID)
		{
			*Result1 = (void *) layer;
			*Result2 = *Result3 = (void *) text;
			return(TEXT_TYPE);
		}
	);

	ALLPOLYGON_LOOP(Base,
		if (polygon->ID == ID)
		{
			*Result1 = (void *) layer;
			*Result2 = *Result3 = (void *) polygon;
			return(POLYGON_TYPE);
		}
		POLYGONPOINT_LOOP(polygon,
			if (point->ID == ID)
			{
				*Result1 = (void *) layer;
				*Result2 = (void *) polygon;
				*Result3 = (void *) point;
				return(POLYGONPOINT_TYPE);
			}
		);
	);

	VIA_LOOP(Base,
		if (via->ID == ID)
		{
			*Result1 = *Result2 = *Result3 = (void *) via;
			return(VIA_TYPE);
		}
	);

		/* check pins and elementnames too */
	ELEMENT_LOOP(Base,
		*Result1 = *Result2 = *Result3 = (void *) element;
		if (element->ID == ID)
		{
			*Result2 = *Result3 = (void *) element;
			return(ELEMENT_TYPE);
		}
		ELEMENTTEXT_LOOP(element,
			if (text->ID == ID)
			{
				*Result2 = *Result3 = (void *) text;
				return(ELEMENTNAME_TYPE);
			}
		);
		PIN_LOOP(element,
			if (pin->ID == ID)
			{
				*Result2 = *Result3 = (void *) pin;
				return(PIN_TYPE);
			}
		);
		PAD_LOOP(element,
			if (pad->ID == ID)
			{
				*Result2 = *Result3 = (void *) pad;
				return(PAD_TYPE);
			}
		);
	);

	return(NO_TYPE);
}
