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
 *  RCS: $Id: crosshair.h,v 143.1 1996/09/16 09:08:32 nau Exp $
 */

/* prototypes for crosshair routines
 */

#ifndef	__CROSSHAIR_INCLUDED__
#define	__CROSSHAIR_INCLUDED__

#include "global.h"

/* ---------------------------------------------------------------------------
 * fits screen coordinates into grid
 */
#define	GRIDFIT_X(x)	((((x) -PCB->GridOffsetX) /PCB->Grid) *PCB->Grid +PCB->GridOffsetX)
#define	GRIDFIT_Y(y)	((((y) -PCB->GridOffsetY) /PCB->Grid) *PCB->Grid +PCB->GridOffsetY)

/* ---------------------------------------------------------------------------
 * all possible states of an attached object
 */
#define	STATE_FIRST		0			/* initial state */
#define	STATE_SECOND	1
#define	STATE_THIRD		2


void	CrosshairOn(Boolean);
void	CrosshairOff(Boolean);
void	HideCrosshair(Boolean);
void	RestoreCrosshair(Boolean);
void	MoveCrosshairRelative(Position, Position);
void	MoveCrosshairAbsolute(Position, Position);
void	SetCrosshairRange(Position, Position, Position, Position);
void	InitCrosshair(void);
void	DestroyCrosshair(void);

#endif
