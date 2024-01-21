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
 *  RCS: $Id: create.h,v 143.1 1996/09/16 09:08:31 nau Exp $
 */

/* prototypes for create routines
 */

#ifndef	__CREATE_INCLUDED__
#define	__CREATE_INCLUDED__

#include "global.h"

DataTypePtr			CreateNewBuffer(void);
PCBTypePtr			CreateNewPCB(Boolean);
PinTypePtr			CreateNewVia(DataTypePtr, Position, Position, Dimension,
						Dimension, char *, int);
LineTypePtr			CreateNewLineOnLayer(LayerTypePtr, Position,
						Position, Position, Position, Dimension, int);
PolygonTypePtr		CreateNewPolygonFromRectangle(LayerTypePtr,
						Position, Position, Position, Position, int);
TextTypePtr			CreateNewText(LayerTypePtr, FontTypePtr, Position, Position,
						BYTE, int, char *, int);
PolygonTypePtr		CreateNewPolygon(LayerTypePtr, int);
PointTypePtr		CreateNewPointInPolygon(PolygonTypePtr, Position, Position);
ElementTypePtr		CreateNewElement(DataTypePtr, ElementTypePtr, FontTypePtr,
						int, char *, char *, char *, Position, Position,
						BYTE, int, int);
LineTypePtr			CreateNewLineInElement(ElementTypePtr, Position, Position,
						Position, Position, Dimension);
ArcTypePtr			CreateNewArcInElement(ElementTypePtr, Position, Position,
						Dimension, Dimension, int, int, Dimension);
PinTypePtr			CreateNewPin(ElementTypePtr, Position, Position, Dimension,
						Dimension, char *, int);
PadTypePtr			CreateNewPad(ElementTypePtr, Position, Position, Position,
						Position, Dimension, char *, int);
LineTypePtr			CreateNewLineInSymbol(SymbolTypePtr, Position, Position,
						Position, Position, Dimension);
void				CreateDefaultFont(void);
RubberbandTypePtr	CreateNewRubberbandEntry(LayerTypePtr,
						LineTypePtr, PointTypePtr);

#endif
