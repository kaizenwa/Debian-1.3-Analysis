/* ########################################################################

			       options.h

   File: options.h
   Path: /home/fournigault/c/X11/xcoral-2.31/options.h
   Description: 
   Created: Fri Jan 27 11:22:31 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 11:22:32 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


/*
 * $Log: options.h,v $
 * Revision 1.2  1993/12/24  14:21:33  klamer
 * Added support for geometry specification for browser and visit window.
 * For this file, that means:
 *  - add members b_x, b_y, b_width, b_height and v_x, v_y, v_width, v_height
 *    for the browser and visit windows.
 *  - add OP_B_* and OP_V_* #defines to be used with GetOpGeo()
 *
 */
#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include <X11/Xlib.h>

/*
 *	Les options utilisables par le programme.
 *	La geometrie, les fontes, les couleurs etc.
 */
typedef struct {
	int 		x, y, width, height;
	int 		b_x, b_y, b_width, b_height;
	int 		v_x, v_y, v_width, v_height;
	unsigned long 	fg, bg;
	unsigned long	text_top_shadow, text_bottom_shadow;
	XFontStruct 	*text_font;
	unsigned long 	mfg, mbg;
	unsigned long	menu_top_shadow, menu_bottom_shadow;
	XFontStruct	*menu_font;
	unsigned long 	cfg, cbg;
	unsigned long	control_top_shadow, control_bottom_shadow;
	unsigned long 	bd;
	unsigned int 	bw;
   	int 		verb;
	char filename [512];
} Options;

#define OP_TEXT_FG	1
#define OP_TEXT_BG	2
#define OP_TEXT_TS	3
#define OP_TEXT_BS	4
#define OP_TEXT_FONT	5

#define OP_MENU_FG	6
#define OP_MENU_BG	7
#define OP_MENU_TS	8
#define OP_MENU_BS	9
#define OP_MENU_FONT	10

#define OP_CTRL_FG	11
#define OP_CTRL_BG	12
#define OP_CTRL_TS	13
#define OP_CTRL_BS	14

#define OP_X		16
#define OP_Y		17
#define OP_WIDTH	18
#define OP_HEIGHT	19

#define OP_B_X		20
#define OP_B_Y		21
#define OP_B_WIDTH	22
#define OP_B_HEIGHT	23

#define OP_V_X		24
#define OP_V_Y		25
#define OP_V_WIDTH	26
#define OP_V_HEIGHT	27

#endif  /* _OPTIONS_H_ */

