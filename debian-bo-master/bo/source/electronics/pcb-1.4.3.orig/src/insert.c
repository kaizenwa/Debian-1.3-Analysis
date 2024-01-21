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

static	char	*rcsid = "$Id: insert.c,v 143.1 1996/09/16 09:08:40 nau Exp $";

/* functions used to insert points into objects
 */

#include "global.h"

#include "copy.h"
#include "create.h"
#include "data.h"
#include "draw.h"
#include "insert.h"
#include "misc.h"
#include "move.h"
#include "select.h"
#include "set.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	*InsertPointIntoLine(LayerTypePtr, LineTypePtr);
static	void	*InsertPointIntoPolygon(LayerTypePtr, PolygonTypePtr);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	Position			InsertX,	/* used by local routines as offset */
							InsertY;
static	PointTypePtr		InsertAt;
static	ObjectFunctionType	InsertFunctions = {
	InsertPointIntoLine,
	NULL,
	InsertPointIntoPolygon,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL };

/* ---------------------------------------------------------------------------
 * inserts a point into a line
 */
static void *InsertPointIntoLine(LayerTypePtr Layer, LineTypePtr Line)
{
	LineType	savedLine;
	PointType	savedPoint = Line->Point2;
	LineTypePtr	line;

		/* save one point of the original line and modify the other one;
		 * it is necessary to restore the 'undo' counter because
		 * 'MoveObject()' and 'CopyObject()' increment it
		 */
	SaveUndoSerialNumber();
	savedLine = *((LineTypePtr) MoveObject(LINEPOINT_TYPE, Layer, Line,
		&Line->Point2, InsertX -savedPoint.X, InsertY -savedPoint.Y));
	RestoreUndoSerialNumber();

		/* create a new line and modify it */
	line = (LineTypePtr) CopyObject(LINE_TYPE, Layer, Line, Line, 0, 0);
	RestoreUndoSerialNumber();
	MoveObject(LINEPOINT_TYPE, Layer, line, &line->Point1,
		savedPoint.X -line->Point1.X, savedPoint.Y -line->Point1.Y);
	RestoreUndoSerialNumber();

	DrawLine(Layer, &savedLine);
	return(line);
}

/* ---------------------------------------------------------------------------
 * inserts a point into a polygon
 */
static void *InsertPointIntoPolygon(LayerTypePtr Layer, PolygonTypePtr Polygon)
{
	PointType	save;
	Cardinal	index,
				n;

	ErasePolygon(Polygon);

		/* first of all get the index where the new point is to be
		 * inserted; this is necessary because 'CreateNewPointInPolygon()'
		 * may change some pointers
		 *
		 * second move all point with a greater index up by one
		 */
	for (index = 0; index < Polygon->PointN; index++)
		if (Polygon->Points[index].ID == InsertAt->ID)
			break;
	save = *CreateNewPointInPolygon(Polygon, InsertX, InsertY);
	for (n = Polygon->PointN-1; n > index; n--)
		Polygon->Points[n] = Polygon->Points[n-1];
	Polygon->Points[index] = save;
	SetPolygonBoundingBox(Polygon);
	DrawPolygon(Layer, Polygon);
	SetChangedFlag(True);
	AddObjectToInsertPointUndoList(POLYGONPOINT_TYPE, Layer, Polygon,
		&Polygon->Points[index]);
	return(Polygon);
}

/* ---------------------------------------------------------------------------
 * inserts point into objects
 */
void *InsertPointIntoObject(int Type, void *Ptr1, void *Ptr2, void *Ptr3,
	Position DX, Position DY)
{
	void	*ptr;

		/* setup offset */
	InsertX = DX;
	InsertY = DY;
	InsertAt = (PointTypePtr) Ptr3;

		/* the operation insert the points to the undo-list */
	ptr = ObjectOperation(&InsertFunctions, Type, Ptr1, Ptr2, Ptr3);
	IncrementUndoSerialNumber();
	return(ptr);
}
