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
 *  RCS: $Id: globalconst.h,v 143.1 1996/09/16 09:09:31 nau Exp $
 */

/* global constants
 * most of these values are also required by files outside the source tree
 * (manuals...)
 */

#ifndef	__GLOBALCONST_INCLUDED__
#define	__GLOBALCONST_INCLUDED__

#include <limits.h>

/* ---------------------------------------------------------------------------
 * some file-, directory- and environment names
 */
#define	EMERGENCY_NAME			"/tmp/PCB.%i.save"		/* %i --> pid */
#define	BACKUP_NAME				"/tmp/PCB.%i.backup"	/* %i --> pid */

/* ---------------------------------------------------------------------------
 * some default values
 */
#define	DEFAULT_SIZE			"7000x5000"	/* default layout size (EURO) */
#define	DEFAULT_MEDIASIZE		"a4"		/* default output media */
#define	DEFAULT_CELLSIZE		50			/* default cell size for symbols */

/* ---------------------------------------------------------------------------
 * frame between the groundplane and the copper
 */
#define	GROUNDPLANEFRAME		20	/* unit == mil */

/* ---------------------------------------------------------------------------
 * some limit specifications
 */
#define	MAX_LAYER				8	/* max number of layer, check source */
									/* code for more changes */
#define	MIN_LINESIZE			1	/* thickness of lines in 1/1000'' */
#define	MAX_LINESIZE			250
#define	MIN_TEXTSCALE			20	/* scaling of text objects in percent */
#define	MAX_TEXTSCALE			1000
#define	MIN_PINORVIASIZE		30	/* size of a pin or via in */
#define	MIN_PINORVIAHOLE		10	/* size of a pins or vias drilling hole */
#define	MAX_PINORVIASIZE		250
#define	MIN_PINORVIACOPPER		20	/* min difference outer-inner diameter */
#define	MIN_PADSIZE				10	/* size of a pad */
#define	MAX_PADSIZE				250
#define	MIN_GRID				1	/* grid in 1/1000'' */
#define	MAX_GRID				200
#define	MIN_ZOOM				0	/* min zoom (shift operations) */
#define	MAX_ZOOM				4	/* max zoom (shift operations) */
#define	MAX_FONTPOSITION		127	/* upper limit of characters in my font */

#define	MAX_COORD				20000	/* coordinate limits */
#define	MIN_SIZE				1000/* lowest widht and height */
#define	MAX_BUFFER				5	/* number of pastebuffers */
									/* additional changes in menu.c are */
									/* also required to select more buffers */

#define	DEFAULT_DRILLINGHOLE	40	/* default inner/outer ratio for */
									/* pins/vias in percent */

#if MAX_LINESIZE > MAX_PINORVIASIZE	/* maximum size value */
#define	MAX_SIZE	MAX_LINESIZE
#else
#define	MAX_SIZE	MAX_PINORVIASIZE
#endif

#ifndef	MAXPATHLEN					/* maximum path length */
#ifdef	PATH_MAX
#define	MAXPATHLEN	PATH_MAX
#else
#define	MAXPATHLEN	2048
#endif
#endif

#define	MAX_LINE_POINT_DISTANCE		15	/* maximum distance when searching */
										/* line points */
#define	MAX_POLYGON_POINT_DISTANCE	25	/* maximum distance when searching */
										/* polygon points */
#define	MAX_ELEMENTNAMES			3	/* number of supported names of */
										/* an element */
#define	MAX_LIBRARY_LINE_LENGTH		256	/* maximum line length in the */
										/* library-description file */
#define	MAX_MODESTACK_DEPTH			16	/* maximum depth of mode stack */
#define	MAX_CROSSHAIRSTACK_DEPTH	16	/* maximum depth of state stack */
#define	MIN_GRID_DISTANCE			4	/* minimum distance between point */
										/* to enable grid drawing */

#endif
