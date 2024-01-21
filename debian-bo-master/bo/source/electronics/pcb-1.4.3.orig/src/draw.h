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
 *  RCS: $Id: draw.h,v 143.1 1996/09/16 09:08:35 nau Exp $
 */

/* prototypes for drawing routines
 */

#ifndef	__DRAW_INCLUDED__
#define	__DRAW_INCLUDED__

#include "global.h"

void	RedrawOutput(void);
void	ClearAndRedrawOutput(void);
void	SwitchDrawingWindow(int, Window, Boolean);
void	RedrawCurrentLayer(void);
void	DrawVia(PinTypePtr);
void	DrawViaName(PinTypePtr);
void	DrawPin(PinTypePtr);
void	DrawPinName(PinTypePtr);
void	DrawPad(PadTypePtr);
void	DrawPadName(PadTypePtr);
void	DrawLine(LayerTypePtr, LineTypePtr);
void	DrawText(LayerTypePtr, TextTypePtr);
void	DrawPolygon(LayerTypePtr, PolygonTypePtr);
void	DrawElement(ElementTypePtr);
void	DrawElementName(ElementTypePtr);
void	DrawElementPackage(ElementTypePtr);
void	DrawElementPinsAndPads(ElementTypePtr);
void	EraseVia(PinTypePtr);
void	EraseViaName(PinTypePtr);
void	ErasePad(PadTypePtr);
void	ErasePadName(PadTypePtr);
void	ErasePin(PinTypePtr);
void	ErasePinName(PinTypePtr);
void	EraseLine(LineTypePtr);
void	EraseText(TextTypePtr);
void	ErasePolygon(PolygonTypePtr);
void	EraseElement(ElementTypePtr);
void	EraseElementPinsAndPads(ElementTypePtr);
void	EraseElementName(ElementTypePtr);
void	DrawGrid(void);

#endif
