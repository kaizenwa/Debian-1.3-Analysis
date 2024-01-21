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

static	char	*rcsid = "$Id: data.c,v 143.1 1996/09/16 09:08:32 nau Exp $";

/* just defines common identifiers
 */
#include "global.h"
#include "dev_ps.h"

/* ---------------------------------------------------------------------------
 * some shared identifiers
 */
XtAppContext	Context;			/* application context */
Display			*Dpy;				/* my display */
CrosshairType	Crosshair;			/* information about cursor settings */
OutputType		Output;				/* some widgets ... used for drawing */
PCBTypePtr		PCB;				/* pointer to layout struct */
char			*Progname;			/* the programs name */
SettingType		Settings;			/* some resources */
Boolean			RedrawOnEnter;		/* a flag, indicating that settings */
									/* have changed */
int				LayerStack[MAX_LAYER];	/* determines the order in */
										/* the layers are drawn */
Atom			WMDeleteWindowAtom;		/* all toplevel windows get */
										/* 'Delete' messages */
BufferType		Buffers[MAX_BUFFER][2];	/* my buffers */
LibraryType		Library;				/* the library */

/* ---------------------------------------------------------------------------
 * set all keys which generate a new line not 'no operation'.
 * 'Return' and 'Escape' are used as accelerators anyway
 */
String	InputTranslations = 
					"<Key>Linefeed: no-op()\n "
					"<Key>Return:   FinishInputDialog(OK)\n "
					"<Key>Escape:   FinishInputDialog(Cancel)\n "
					"Ctrl<Key>j:    no-op()\n "
					"Ctrl<Key>m:    no-op()\n "
					"Ctrl<Key>o:    no-op()\n ";

/* ---------------------------------------------------------------------------
 * all known printing devices
 */
DeviceInfoType	PrintingDevice[2] = {
	{ PS_Query, NULL },
	{ EPS_Query, NULL }};

