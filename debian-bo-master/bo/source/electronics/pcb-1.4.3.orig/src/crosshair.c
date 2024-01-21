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

static	char	*rcsid = "$Id: crosshair.c,v 143.1 1996/09/16 09:08:32 nau Exp $";

/* crosshair stuff
 */

#include <memory.h>

#include "global.h"

#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "error.h"
#include "mymem.h"

#include <X11/Xaw/Simple.h>

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Boolean		CrosshairStack[MAX_CROSSHAIRSTACK_DEPTH];
static	int			CrosshairStackPosition = 0;
static	XPoint		*Points = NULL;		/* data of tmp polygon */
static	Cardinal	MaxPoints = 0;				/* number of points */

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	CreateTMPPolygon(PolygonTypePtr, Position, Position);
static	void	DrawCrosshair(void);
static	void	XORDrawElement(ElementTypePtr, Position, Position);
static	void	XORDrawBuffer(BufferTypePtr);
static	void	XORDrawInsertPointObject(void);
static	void	XORDrawMoveOrCopyObject(void);
static	void	DrawAttached(Boolean);
static	void	FitCrosshairIntoGrid(Position, Position);

/* ---------------------------------------------------------------------------
 * creates a tmp polygon with coordinates converted to screen system
 */
static void CreateTMPPolygon(PolygonTypePtr Polygon, Position DX, Position DY)
{
		/* allocate memory for data with screen coordinates */
	if (Polygon->PointN >= MaxPoints)
	{
			/* allocate memory for one additional point */
		MaxPoints = Polygon->PointN +1;
		Points = (XPoint *) MyRealloc(Points,
			MaxPoints *sizeof(XPoint), "CreateTMPPolygon()");
	}

		/* copy data to tmp array and convert it to screen coordinates */
	POLYGONPOINT_LOOP(Polygon,
		Points[n].x = TO_SCREEN_X(point->X +DX);
		Points[n].y = TO_SCREEN_Y(point->Y +DY);
	);

		/* the last point is identical to the first one */
	Points[Polygon->PointN].x = Points[0].x;
	Points[Polygon->PointN].y = Points[0].y;
}

/* ---------------------------------------------------------------------------
 * draws the crosshair 
 * don't transform MAX_COORD to screen coordinats, it is
 * already the maximum of screen- and pcb-coordinates
 */
static void DrawCrosshair(void)
{
	XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
		TO_SCREEN_X(Crosshair.X), 0, TO_SCREEN_X(Crosshair.X), MAX_COORD);
	XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
		0, TO_SCREEN_Y(Crosshair.Y), MAX_COORD, TO_SCREEN_Y(Crosshair.Y));
}

/* ---------------------------------------------------------------------------
 * draws the elements of a loaded circuit which is to be merged in
 */
static void XORDrawElement(ElementTypePtr Element, Position DX, Position DY)
{
	ELEMENTLINE_LOOP(Element,
		XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
			TO_SCREEN_X(DX +line->Point1.X), TO_SCREEN_Y(DY +line->Point1.Y),
			TO_SCREEN_X(DX +line->Point2.X), TO_SCREEN_Y(DY +line->Point2.Y));
	);

		/* arc coordinates and angles have to be converted to X11 notation */
	ARC_LOOP(Element,
		XDrawArc(Dpy, Output.OutputWindow, Crosshair.GC,
			TO_SCREEN_X(DX +arc->X) -TO_SCREEN(arc->Width),
			TO_SCREEN_Y(DY +arc->Y) -TO_SCREEN(arc->Height),
			TO_SCREEN(2*arc->Width), TO_SCREEN(2*arc->Height),
			(TO_SCREEN_ANGLE(arc->StartAngle) +180) *64,
			TO_SCREEN_DELTA(arc->Delta) *64);
	);

		/* pin coordinates and angles have to be converted to X11 notation */
	PIN_LOOP(Element,
		XDrawArc(Dpy, Output.OutputWindow, Crosshair.GC,
			TO_SCREEN_X(DX +pin->X) -TO_SCREEN(pin->Thickness/2),
			TO_SCREEN_Y(DY +pin->Y) -TO_SCREEN(pin->Thickness/2),
			TO_SCREEN(pin->Thickness), TO_SCREEN(pin->Thickness),
			0, 360*64);
	);

		/* pads */
	PAD_LOOP(Element,
		if ((TEST_FLAG(ONSOLDERFLAG, pad) != 0) == Settings.ShowSolderSide ||
			PCB->InvisibleObjectsOn)
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(DX +pad->Point1.X), TO_SCREEN_Y(DY +pad->Point1.Y),
				TO_SCREEN_X(DX +pad->Point2.X), TO_SCREEN_Y(DY +pad->Point2.Y));
	);

}

/* ---------------------------------------------------------------------------
 * draws all visible and attached objects of the pastebuffer
 */
static void XORDrawBuffer(BufferTypePtr Buffer)
{
	Cardinal	i;
	Position	x, y;

		/* set offset */
	x = Crosshair.X -Buffer->X;
	y = Crosshair.Y -Buffer->Y;

		/* draw all visible layers */
	for (i = 0; i < MAX_LAYER; i++)
		if (PCB->Data->Layer[i].On)
		{
			LayerTypePtr	layer = &Buffer->Data->Layer[i];

			LINE_LOOP(layer,
				XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
					TO_SCREEN_X(x +line->Point1.X), TO_SCREEN_Y(y +line->Point1.Y),
					TO_SCREEN_X(x +line->Point2.X), TO_SCREEN_Y(y +line->Point2.Y));
			);
			TEXT_LOOP(layer,
				{
					BoxTypePtr	box = &text->BoundingBox;

					XDrawRectangle(Dpy, Output.OutputWindow, Crosshair.GC,
						TO_SCREEN_X(x +box->X1), TO_SCREEN_Y(y +box->Y1),
						TO_SCREEN(box->X2 -box->X1),
						TO_SCREEN(box->Y2 -box->Y1));
				}
			);
				/* the tmp polygon has n+1 points because the first
				 * and the last one are set to the same coordinates
				 */
			POLYGON_LOOP(layer,
				{
					CreateTMPPolygon(polygon, x, y);
					XDrawLines(Dpy, Output.OutputWindow, Crosshair.GC,
						Points, polygon->PointN+1, CoordModeOrigin);
				}
			);
		}

		/* draw elements if visible */
	if (PCB->PinOn || PCB->ElementOn)
		ELEMENT_LOOP(Buffer->Data,
			if ((TEST_FLAG(ONSOLDERFLAG, element) != 0) == SWAP_IDENT ||
			PCB->InvisibleObjectsOn)
				XORDrawElement(element, x, y);
		);

		/* and the vias, move offset by thickness/2 */
	if (PCB->ViaOn)
		VIA_LOOP(Buffer->Data,
			XDrawArc(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(x +via->X -via->Thickness/2),
				TO_SCREEN_Y(y +via->Y -via->Thickness/2),
				TO_SCREEN(via->Thickness), TO_SCREEN(via->Thickness),
				0, 360*64);
		);
}

/* ---------------------------------------------------------------------------
 * draws the rubberband to insert points into polygons/lines/...
 */
static void XORDrawInsertPointObject(void)
{
		/* decicion is based onthe parent object */
	switch(Crosshair.AttachedObject.Type)
	{
		case LINE_TYPE:
		{
			LineTypePtr	line = (LineTypePtr) Crosshair.AttachedObject.Ptr2;

			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y),
				TO_SCREEN_X(line->Point1.X), TO_SCREEN_Y(line->Point1.Y));
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y),
				TO_SCREEN_X(line->Point2.X), TO_SCREEN_Y(line->Point2.Y));
		}
		break;

		case POLYGON_TYPE:
		{
			PolygonTypePtr	polygon;
			PointTypePtr	first,
							second;
			
			polygon = (PolygonTypePtr) Crosshair.AttachedObject.Ptr2;
			second = (PointTypePtr) Crosshair.AttachedObject.Ptr3;

				/* take care about overflow */
			first = (second == polygon->Points ?
				&polygon->Points[polygon->PointN-1] : second -1);
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y),
				TO_SCREEN_X(first->X), TO_SCREEN_Y(first->Y));
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(Crosshair.X), TO_SCREEN_Y(Crosshair.Y),
				TO_SCREEN_X(second->X), TO_SCREEN_Y(second->Y));
		}
		break;
	}
}

/* ---------------------------------------------------------------------------
 * draws the attched object while in RUBBERBAND_MOVE, MOVE_MODE or COPY_MODE
 */
static void XORDrawMoveOrCopyObject(void)
{
	RubberbandTypePtr	ptr;
	Cardinal			i;
	Position			dx = Crosshair.X -Crosshair.AttachedObject.X,
						dy = Crosshair.Y -Crosshair.AttachedObject.Y;

	switch(Crosshair.AttachedObject.Type)
	{
		case VIA_TYPE:
		{
			PinTypePtr	via = (PinTypePtr) Crosshair.AttachedObject.Ptr1;

			XDrawArc(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(via->X +dx -via->Thickness/2),
				TO_SCREEN_Y(via->Y +dy -via->Thickness/2),
				TO_SCREEN(via->Thickness), TO_SCREEN(via->Thickness),0,360*64);
			break;
		}

		case LINE_TYPE:
		{
			LineTypePtr	line = (LineTypePtr) Crosshair.AttachedObject.Ptr2;

			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(line->Point1.X +dx), TO_SCREEN_Y(line->Point1.Y +dy),
				TO_SCREEN_X(line->Point2.X +dx), TO_SCREEN_Y(line->Point2.Y +dy));
			break;
		}

		case POLYGON_TYPE:
		{
			PolygonTypePtr	polygon = (PolygonTypePtr) Crosshair.AttachedObject.Ptr2;

				/* the tmp polygon has n+1 points because the first
				 * and the last one are set to the same coordinates
				 */
			CreateTMPPolygon(polygon, dx, dy);
			XDrawLines(Dpy, Output.OutputWindow, Crosshair.GC,
				Points, polygon->PointN +1, CoordModeOrigin);
			break;
		}

		case LINEPOINT_TYPE:
		{
			LineTypePtr		line;
			PointTypePtr	point;

			line = (LineTypePtr) Crosshair.AttachedObject.Ptr2;
			point = (PointTypePtr) Crosshair.AttachedObject.Ptr3;
			if (point == &line->Point1)
				XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
					TO_SCREEN_X(point->X +dx), TO_SCREEN_Y(point->Y +dy),
					TO_SCREEN_X(line->Point2.X), TO_SCREEN_Y(line->Point2.Y));
			else
				XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
					TO_SCREEN_X(point->X +dx), TO_SCREEN_Y(point->Y +dy),
					TO_SCREEN_X(line->Point1.X), TO_SCREEN_Y(line->Point1.Y));
			break;
		}

		case POLYGONPOINT_TYPE:
		{
			PolygonTypePtr	polygon;
			PointTypePtr	point,
							previous,
							following;

			polygon = (PolygonTypePtr) Crosshair.AttachedObject.Ptr2;
			point = (PointTypePtr) Crosshair.AttachedObject.Ptr3;

				/* get previous and following point */
			if (point == polygon->Points)
			{
				previous = &polygon->Points[polygon->PointN-1];
				following = point+1;
			}
			else
				if (point == &polygon->Points[polygon->PointN-1])
				{
					previous = point-1;
					following = &polygon->Points[0];
				}
				else
				{
					previous = point-1;
					following = point+1;
				}

				/* draw the two segments */
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(previous->X), TO_SCREEN_Y(previous->Y),
				TO_SCREEN_X(point->X +dx), TO_SCREEN_Y(point->Y +dy));
			XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(point->X +dx), TO_SCREEN_Y(point->Y +dy),
				TO_SCREEN_X(following->X), TO_SCREEN_Y(following->Y));
			break;
		}

			/* element names are moved like normal text objects */
		case TEXT_TYPE:
		case ELEMENTNAME_TYPE:
		{
			TextTypePtr	text = (TextTypePtr) Crosshair.AttachedObject.Ptr2;
			BoxTypePtr	box = &text->BoundingBox;
			int			x0, y0;

			x0 = box->X1;
			y0 = Settings.ShowSolderSide ? box->Y2 : box->Y1;
			XDrawRectangle(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(x0 +dx), TO_SCREEN_Y(y0 +dy),
				TO_SCREEN(box->X2 -box->X1), TO_SCREEN(box->Y2 -box->Y1));
			break;
		}

			/* pin/pad movements result in moving an element */
		case PAD_TYPE:
		case PIN_TYPE:
		case ELEMENT_TYPE:
			XORDrawElement((ElementTypePtr) Crosshair.AttachedObject.Ptr2,
				dx, dy);
			break;
	}

		/* draw the attached rubberband lines too */
	i = Crosshair.AttachedObject.RubberbandN;
	ptr = Crosshair.AttachedObject.Rubberband;
	while (i)
	{
		PointTypePtr	point1,
						point2;;

			/* 'point1' is always the fix-point */
		if (ptr->MovedPoint == &ptr->Line->Point1)
		{
			point1 = &ptr->Line->Point2;
			point2 = &ptr->Line->Point1;
		}
		else
		{
			point1 = &ptr->Line->Point1;
			point2 = &ptr->Line->Point2;
		}
		XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
			TO_SCREEN_X(point1->X), TO_SCREEN_Y(point1->Y),
			TO_SCREEN_X(point2->X +dx), TO_SCREEN_Y(point2->Y +dy));
		ptr++;
		i--;
	}
}

/* ---------------------------------------------------------------------------
 * draws additional stuff that follows the crosshair
 */
static void DrawAttached(Boolean BlockToo)
{
	DrawCrosshair();
	switch (Settings.Mode)
	{
		case VIA_MODE:
			XDrawArc(Dpy, Output.OutputWindow, Crosshair.GC,
				TO_SCREEN_X(Crosshair.X -Settings.ViaThickness/2),
				TO_SCREEN_Y(Crosshair.Y -Settings.ViaThickness/2),
				TO_SCREEN(Settings.ViaThickness),
				TO_SCREEN(Settings.ViaThickness),
				0, 360*64);
			break;
			
			/* the attached line is used by both LINEMODE and POLYGON_MODE */
		case LINE_MODE:
		case POLYGON_MODE:
				/* draw only if starting point is set */
			if (Crosshair.AttachedLine.State != STATE_FIRST)
				XDrawLine(Dpy, Output.OutputWindow, Crosshair.GC,
					TO_SCREEN_X(Crosshair.AttachedLine.Point1.X),
					TO_SCREEN_Y(Crosshair.AttachedLine.Point1.Y),
					TO_SCREEN_X(Crosshair.AttachedLine.Point2.X),
					TO_SCREEN_Y(Crosshair.AttachedLine.Point2.Y));

				/* draw attached polygon only if in POLYGON_MODE */
			if (Settings.Mode == POLYGON_MODE &&
				Crosshair.AttachedPolygon.PointN > 1)
			{
				CreateTMPPolygon(&Crosshair.AttachedPolygon, 0, 0);
				XDrawLines(Dpy, Output.OutputWindow, Crosshair.GC,
					Points, Crosshair.AttachedPolygon.PointN,
					CoordModeOrigin);
			}
			break;

		case PASTEBUFFER_MODE:
			XORDrawBuffer(PASTEBUFFER);
			break;

		case COPY_MODE:
		case MOVE_MODE:
		case RUBBERBANDMOVE_MODE:
			XORDrawMoveOrCopyObject();
			break;

		case INSERTPOINT_MODE:
			XORDrawInsertPointObject();
			break;
	}

		/* an attached box does not depend on a special mode */
	if (Crosshair.AttachedBox.State == STATE_SECOND ||
		(BlockToo && Crosshair.AttachedBox.State == STATE_THIRD))
	{
		Position    x1, y1,
					x2, y2,
					y0;

		x1 = MIN(Crosshair.AttachedBox.Point1.X,Crosshair.AttachedBox.Point2.X);
		y1 = MIN(Crosshair.AttachedBox.Point1.Y,Crosshair.AttachedBox.Point2.Y);
		x2 = MAX(Crosshair.AttachedBox.Point1.X,Crosshair.AttachedBox.Point2.X);
		y2 = MAX(Crosshair.AttachedBox.Point1.Y,Crosshair.AttachedBox.Point2.Y);
		y0 = Settings.ShowSolderSide ? y2 : y1;
		XDrawRectangle(Dpy, Output.OutputWindow, Crosshair.GC,
			TO_SCREEN_X(x1), TO_SCREEN_Y(y0),
			TO_SCREEN(x2-x1), TO_SCREEN(y2-y1));
	}
}

/* ---------------------------------------------------------------------------
 * switches crosshair on
 */
void CrosshairOn(Boolean BlockToo)
{
	if (!Crosshair.On)
	{
		Crosshair.On = True;
		DrawAttached(BlockToo);
	}
}

/* ---------------------------------------------------------------------------
 * switches crosshair off
 */
void CrosshairOff(Boolean BlockToo)
{
	if (Crosshair.On)
	{
		Crosshair.On = False;
		DrawAttached(BlockToo);
	}
}

/* ---------------------------------------------------------------------------
 * saves crosshair state (on/off) and hides him
 */
void HideCrosshair(Boolean BlockToo)
{
	CrosshairStack[CrosshairStackPosition++] = Crosshair.On;
	if (CrosshairStackPosition >= MAX_CROSSHAIRSTACK_DEPTH)
		CrosshairStackPosition--;
	CrosshairOff(BlockToo);
}

/* ---------------------------------------------------------------------------
 * restores last crosshair state
 */
void RestoreCrosshair(Boolean BlockToo)
{
	if (CrosshairStackPosition)
	{
		if (CrosshairStack[--CrosshairStackPosition])
			CrosshairOn(BlockToo);
		else
			CrosshairOff(BlockToo);
	}
}

/* ---------------------------------------------------------------------------
 * recalculates the passed coordinates to fit the current grid setting
 */
static void FitCrosshairIntoGrid(Position X, Position Y)
{
	Position	x1, y1, x2, y2,
				dx1, dy1, dx2, dy2;
	
		/* get PCB coordinates from visible display size.
		 * If the bottom view mode is active, y2 might be less then y1
		 */
	dx1 = TO_PCB_X(Output.OffsetX);
	dy1 = TO_PCB_Y(Output.OffsetY);
	dx2 = TO_PCB_X(Output.OffsetX +Output.Width -1);
	dy2 = TO_PCB_Y(Output.OffsetY +Output.Height -1);
	x1 = MIN(dx1, dx2);
	y1 = MIN(dy1, dy2);
	x2 = MAX(dx1, dx2);
	y2 = MAX(dy1, dy2);

		/* check position agains window size and agains valid
		 * coordinates determined by the size of an attached
		 * object or buffer
		 */
	Crosshair.X = MIN(x2, MAX(x1, X));
	Crosshair.Y = MIN(y2, MAX(y1, Y));
	Crosshair.X = MIN(Crosshair.MaxX, MAX(Crosshair.MinX, Crosshair.X));
	Crosshair.Y = MIN(Crosshair.MaxY, MAX(Crosshair.MinY, Crosshair.Y));
	
		/* check if new position is inside the output window
		 * This might not be true after the window has been resized.
		 * In this case we just set it to the center of the window or
		 * with respect to the grid (if possible)
		 */
	if (Crosshair.X < x1 || Crosshair.X > x2)
	{
		if (x2 -x1 +1 >= PCB->Grid)
				/* there must be a point that matches the grid 
				 * so we just have to look for it with some integer
				 * calculations
				 */
			Crosshair.X = GRIDFIT_X(x1 +PCB->Grid);
		else
			Crosshair.X = (x1+x2)/2;
	}
	else
			/* check if the new position matches the grid */
		Crosshair.X = GRIDFIT_X(Crosshair.X);

		/* to the same for the second coordinate */
	if (Crosshair.Y < y1 || Crosshair.Y > y2)
	{
		if (y2 -y1 +1 >= PCB->Grid)
			Crosshair.Y = GRIDFIT_Y(y1 +PCB->Grid);
		else
			Crosshair.Y = (y1+y2)/2;
	}
	else
		Crosshair.Y = GRIDFIT_Y(Crosshair.Y);
}

/* ---------------------------------------------------------------------------
 * move crosshair relative (has to be switched off)
 */
void MoveCrosshairRelative(Position DeltaX, Position DeltaY)
{
	FitCrosshairIntoGrid(Crosshair.X +DeltaX, Crosshair.Y +DeltaY);
}

/* ---------------------------------------------------------------------------
 * move crosshair absolute (has to be switched off)
 */
void MoveCrosshairAbsolute(Position X, Position Y)
{
	FitCrosshairIntoGrid(X, Y);
}

/* ---------------------------------------------------------------------------
 * sets the valid range for the crosshair cursor
 */
void SetCrosshairRange(Position MinX, Position MinY,
	Position MaxX, Position MaxY)
{
	Crosshair.MinX = MAX(0, MinX);
	Crosshair.MinY = MAX(0, MinY);
	Crosshair.MaxX = MIN((Position) PCB->MaxWidth, MaxX);
	Crosshair.MaxY = MIN((Position) PCB->MaxHeight, MaxY);

		/* force update of position */
	MoveCrosshairRelative(0, 0);
}

/* ---------------------------------------------------------------------------
 * initializes crosshair stuff
 * clears the struct, allocates to graphical contexts and
 * initializes the stack
 */
void InitCrosshair(void)
{
		/* clear struct */
	memset(&Crosshair, 0, sizeof(CrosshairType));

	Crosshair.GC = XCreateGC(Dpy, Output.OutputWindow, 0, NULL);
	if (!VALID_GC((int) Crosshair.GC))
		MyFatal("can't create default crosshair GC\n");
	XSetState(Dpy, Crosshair.GC, Settings.CrosshairColor, Settings.bgColor,
		GXxor, AllPlanes);

		/* fake an crosshair off entry on stack */
	CrosshairStackPosition = 0;
	CrosshairStack[CrosshairStackPosition++] = True;
	Crosshair.On = False;

		/* set default limits */
	Crosshair.MinX = Crosshair.MinY = 0;
	Crosshair.MaxX = PCB->MaxWidth;
	Crosshair.MaxY = PCB->MaxHeight;
}

/* ---------------------------------------------------------------------------
 * exits crosshair routines, release GCs
 */
void DestroyCrosshair(void)
{
	CrosshairOff(True);
	FreePolygonMemory(&Crosshair.AttachedPolygon);
	XFreeGC(Dpy, Crosshair.GC);
}

