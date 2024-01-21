/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * a copy of which is included here in file "GNU_GENERAL"
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations or by individuals for personal HOME use. 
 * This software, or any of its parts, may not be used by for-profit
 * organization, regardless of application or intended product or
 * customer, without the permission of the Board of Regents of the 
 * University  of Wisconsin. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */
#define BORDER	1
/* #define FONT	"fixed" */
#define Max(a, b) ((a)>(b) ? (a) : (b))
#define Min(a, b) ((a)<(b) ? (a) : (b))
#define RECT_X   100
#define RECT_Y   100
#define RECT_W   450
#define RECT_H   350
#define WIN_W    650
#define WIN_H    550


   Window      win;		/* Window ID */
   unsigned long pad;	        /* Font size parameter */
   unsigned long fg, bg, bd;	/* Pixel values */
   unsigned long bw;		/* Border width */
   XEvent      event;		/* Event received */
   XSizeHints  xsh;		/* Size hints for window manager */
   XSetWindowAttributes xswa;	/* Temporary Set Window Attribute struct */
   XWindowAttributes xwa;	/* Temporary Window Attribute struct */


extern double pow10();
extern char *rm_tail_zero();
extern void DrawOpenSquare();
extern void DrawCloseSquare();
extern void DrawOpenDiamon();
extern void DrawCloseDiamon();
extern void DrawCross();
extern void DrawOpenTriangle();

extern double Lpow10();
extern char *Lrm_tail_zero();
extern void LDrawOpenSquare();
extern void LDrawCloseSquare();
extern void LDrawOpenDiamon();
extern void LDrawCloseDiamon();
extern void LDrawCross();
extern void LDrawOpenTriangle();
