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

/* This file declares the functions and globals defined in scXstuff.c */
/* REVISION HISTORY */
/* 7-19-91   B. Backman   Creation */

#include <curses.h>
#ifdef HAVE_X11_X_H
#include <X11/Xutil.h>
#include <X11/Xlib.h>

Display    *dpy;		/* X server connection */
extern Window     mainwin;	/* Window ID of main window*/
extern GC         maingc,	/* GC for mainwin */
                maingcreversed,	/* reverse-field GC for mainwin */
	          invertgc;	/* (invert) reverse-field GC for mainwin */
XFontStruct *curfont; 	/* Font descriptor structure for current font */
int curfontwidth, 
	   curfontheight;    /* dimensions of current font (in pixels) */
char *userfont;			/*  user specified font name */
char foreg[30];
char backg[30];
#endif /* HAVE_X11_X_H */

/* macros textrow() and textcol() compute the y-coordinate of the bottom of row
   r and the x-coordinate of the left-hand side of column c, respectively. 
   This is for use with XDrawImageString.  The coordinates are based on 
   curfontheight and curfontwidth.  NOTE: textcol() will only work for a 
   fixed-width font! Otherwise, it doesn't make sense to calculate column 
   positions anyway because they change */
#define textrow(r)  ((((r)+1)*curfontheight)-1)
#define textcol(c)  ((c)*curfontwidth)


/* functions defined in scXstuff.c  */
extern void	cleardownfrom PROTO((int));
extern void	clearupfrom PROTO((int));
extern int	sc_Xinit PROTO((int, char **));
extern void	sc_handleresize PROTO((XEvent *));
extern void	usefont PROTO((XFontStruct *));


/* functions defined in graphic_main.c  */
extern void	graphic_init PROTO((void));
extern void	plot_XY PROTO((void));
extern void	plot_bar PROTO((void));
extern void	plot_line PROTO((void));
extern void	plot_pie PROTO((void));
extern void	plot_stacked_bar PROTO((void));

