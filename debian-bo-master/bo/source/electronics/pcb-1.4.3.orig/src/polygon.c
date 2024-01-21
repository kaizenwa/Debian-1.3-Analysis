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

static	char	*rcsid = "$Id: polygon.c,v 143.1 1996/09/16 09:08:51 nau Exp $";

/* special polygon editing routines
 */

#include <math.h>
#include <memory.h>

#include "global.h"

#include "create.h"
#include "crosshair.h"
#include "data.h"
#include "draw.h"
#include "error.h"
#include "misc.h"
#include "move.h"
#include "polygon.h"
#include "remove.h"
#include "search.h"
#include "set.h"
#include "undo.h"

/* ---------------------------------------------------------------------------
 * returns the index of the polygon point which is the end
 * point of the segment with the lowest distance to the passed
 * coordinates
 */
PointTypePtr GetLowestDistancePolygonPoint(PolygonTypePtr Polygon,
	Position X, Position Y)
{
	float			mindistance = MAX_COORD,
					length,
					distance;
	PointTypePtr	ptr1 = &Polygon->Points[Polygon->PointN-1],
					ptr2 = &Polygon->Points[0],
					result = ptr2;
	Cardinal		n;

		/* get segment next to crosshair location;
		 * it is determined by the last point of it
		 */
	for (n = 0; n < Polygon->PointN; n++, ptr2++)
	{

		length = sqrt((ptr2->X -ptr1->X)*(ptr2->X -ptr1->X) +
			(ptr2->Y -ptr1->Y)*(ptr2->Y -ptr1->Y));
		if (length != 0.0)
		{
			distance = fabs(((ptr2->Y -ptr1->Y)*(Crosshair.X -ptr1->X)-
				(ptr2->X -ptr1->X)*(Crosshair.Y -ptr1->Y))/length);
			if (distance < mindistance)
			{
				mindistance = distance;
				result = ptr2;
			}
		}
		ptr1 = ptr2;
	}
	return(result);
}

/* ---------------------------------------------------------------------------
 * go back to the  previous point of the polygon
 */
void GoToPreviousPoint(void)
{
	switch(Crosshair.AttachedPolygon.PointN)
	{
			/* do nothing if mode has just been entered */
		case 0:
			break;

			/* reset number of points and 'LINE_MODE' state */
		case 1:
			Crosshair.AttachedPolygon.PointN = 0;
			Crosshair.AttachedLine.State = STATE_FIRST;
			break;

			/* back-up one point */
		default:
		{
			PointTypePtr	points = Crosshair.AttachedPolygon.Points;
			Cardinal		n = Crosshair.AttachedPolygon.PointN -2;
	
			Crosshair.AttachedPolygon.PointN--;
			Crosshair.AttachedLine.Point1.X = points[n].X;
			Crosshair.AttachedLine.Point1.Y = points[n].Y;
			break;
		}
	}
}

/* ---------------------------------------------------------------------------
 * close polygon if possible
 */
void ClosePolygon(void)
{
	Cardinal	n = Crosshair.AttachedPolygon.PointN;

		/* check number of points */
	if (n >= 3)
	{
			/* if 45 degree lines are what we want do a quick check
			 * if closing the polygon makes sense
			 */
		if (!TEST_FLAG(ALLDIRCETIONFLAG, PCB))
		{
			Dimension	dx, dy;

			dx = abs(Crosshair.AttachedPolygon.Points[n-1].X -
				Crosshair.AttachedPolygon.Points[0].X);
			dy = abs(Crosshair.AttachedPolygon.Points[n-1].Y -
				Crosshair.AttachedPolygon.Points[0].Y);
			if (!(dx == 0 || dy == 0 || dx == dy))
			{
				Message("cannot close polygon because 45 degree lines are requested\n");
				return;
			}
		}
		CopyAttachedPolygonToLayer();
	}
	else
		Message("a polygon has to have at least 3 points\n");
}

/* ---------------------------------------------------------------------------
 * moves the data of the attached (new) polygon to the current layer
 */
void CopyAttachedPolygonToLayer(void)
{
	PolygonTypePtr	polygon;
	
		/* move data to layer and clear attached struct */
	polygon = CreateNewPolygon(CURRENT, NOFLAG);
	*polygon = Crosshair.AttachedPolygon;
	memset(&Crosshair.AttachedPolygon, 0, sizeof(PolygonType));
	SetPolygonBoundingBox(polygon);
	DrawPolygon(CURRENT, polygon);
	SetChangedFlag(True);

		/* reset state of attached line */
	Crosshair.AttachedLine.State = STATE_FIRST;

		/* add to undo list */
	AddObjectToCreateUndoList(POLYGON_TYPE, CURRENT, polygon, polygon);
	IncrementUndoSerialNumber();
}

