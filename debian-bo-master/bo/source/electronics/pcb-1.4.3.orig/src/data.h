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
 *  RCS: $Id: data.h,v 143.1 1996/09/16 09:08:33 nau Exp $
 */

/* common identifiers
 */

#ifndef	__DATA_INCLUDED__
#define	__DATA_INCLUDED__

#include "global.h"
#include <X11/Intrinsic.h>

/* ---------------------------------------------------------------------------
 * some shared identifiers
 */
extern	XtAppContext		Context;
extern	Display				*Dpy;
extern	CrosshairType		Crosshair;
extern	OutputType			Output;
extern	PCBTypePtr			PCB;
extern	char				*Progname;
extern	SettingType			Settings;
extern	Boolean				RedrawOnEnter;
extern	int					LayerStack[MAX_LAYER];
extern	String				InputTranslations;
extern	Atom				WMDeleteWindowAtom;
extern	BufferType			Buffers[MAX_BUFFER][2];
extern	LibraryType			Library;
extern	DeviceInfoType		PrintingDevice[2];

#endif
