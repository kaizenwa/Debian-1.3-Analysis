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
 *  RCS: $Id: change.h,v 143.1 1996/09/16 09:08:28 nau Exp $
 */

/* prototypes to change object properties
 */

#ifndef	__CHANGE_INCLUDED__
#define	__CHANGE_INCLUDED__

#include "global.h"

/* ---------------------------------------------------------------------------
 * some defines
 */
#define	CHANGENAME_TYPES        \
	(VIA_TYPE | PIN_TYPE | PAD_TYPE | TEXT_TYPE | ELEMENT_TYPE)

#define	CHANGESIZE_TYPES        \
	(VIA_TYPE | PIN_TYPE | PAD_TYPE | LINE_TYPE | TEXT_TYPE | ELEMENTNAME_TYPE)

#define	CHANGE2NDSIZE_TYPES     \
	(VIA_TYPE | PIN_TYPE)

#define	CHANGESQUARE_TYPES     \
	(ELEMENT_TYPE | PIN_TYPE)

Boolean	ChangeLayoutName(char *);
Boolean	ChangeLayerName(LayerTypePtr, char *);
Boolean	ChangeSelectedPinSize(Position);
Boolean	ChangeSelectedPadSize(Position);
Boolean	ChangeSelectedPin2ndSize(Position);
Boolean	ChangeSelectedViaSize(Position);
Boolean	ChangeSelectedVia2ndSize(Position);
Boolean	ChangeSelectedLineSize(Position); 
Boolean	ChangeSelectedTextSize(Position); 
Boolean	ChangeSelectedPinSquare(void); 
Boolean	ChangeSelectedElementSquare(void); 
Boolean	ChangeObjectSize(int, void *, void *, void *, Position);
Boolean	ChangeObject2ndSize(int, void *, void *, void *, Position);
Boolean	ChangeObjectSquare(int, void *, void *, void *);
void	*ChangeObjectName(int, void *, void *, void *, char *);
void	*QueryInputAndChangeObjectName(int, void *, void *, void *);

#endif

