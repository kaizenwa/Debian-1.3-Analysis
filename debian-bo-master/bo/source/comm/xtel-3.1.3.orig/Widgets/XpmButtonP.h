/*	
 *   Copyright (C) 1995  Pierre Ficheux (pierre@rd.lectra.fr)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ident "$Id: XpmButtonP.h,v 1.1 1996/09/14 22:27:47 pierre Exp $"

/* 
 * XpmButtonP.h - Private definitions for XpmButton widget
 */

#ifndef _XpmButtonP_h
#define _XpmButtonP_h

#ifdef USE_MOTIF
#include <Xm/PrimitiveP.h>
#else
#include <X11/Xaw/SimpleP.h>
#endif
#include <X11/xpm.h>

#include "XpmButton.h"

/* XPM */
static char * default_xpm[] = {
"31 56 10 1",
" 	c white",
".	c sienna",
"X	c peru",
"o	c wheat",
"O	c black",
"+	c lemon chiffon",
"@	c slate grey",
"#	c red",
"$	c tan",
"%	c green",
"             .......XX         ",
"           ..XXXXX...XX        ",
"          ..XX........X.       ",
"         .XXX..oooooo....      ",
"         XX.XoooooooooX .      ",
"         ....oOOOooOOOoX..     ",
"         .X..o O oo O oX...    ",
"         .X.ooO+OooO+OoX.X.    ",
"        ..X.ooo++Oo+++oX.XX.   ",
"        ..X.Xoo+++Oo+oX...X..  ",
"        ..X.X@@oooooo@X...XX.  ",
"      OO.XX..Xoo#OO#oX.XX......",
"   OOO..XX....Xoo##ooX..XXXX...",
"  O....XX...X..XooooXX.....XX..",
"O...............@@@.....oooOXX.",
"    ..O...XXX..o@@...X..OoooO..",
"  O..O...XX...o..XX..X..oooooO.",
"  O.OXX.XX...oo.XX.o.X.$ooooooO",
" O..XX..o..o.o....o..X..ooooooo",
"O.X....o.XXoo..oooo..X.O$oooooo",
"O.OO.Oo....oo.oo+++o.XXXO$ooooo",
" O .OO%o+++oooo+++++o..XOO$$ooo",
"  . %%%%++++oO++++++%%..OOO$$oo",
"   O%%%%%%%oO+++%%%%%%%%O$O$$oo",
"   %%%%%%%%%O%%%%%%%%%%%O$OO$$o",
"  %%%%%%%%%%O%%%%%%%%%%%O$$..$$",
"  %%%%%%%%%%%%%%%%%%%%%%O$$.X.$",
"   %%%%%%%%%%%%%%%%%%%%%O$..X..",
"    %%%%%%%%%%%%%%%%%%%O$$..XX.",
"     O%%%OO$OO%%%%%%%%OO$$...X.",
"      OOO$$$$$OO%%%%%OO$$$.X.X.",
"      OXO$$ooo$$OOOOO$$$o..X...",
"      OX.$ooooo$$$$$$$$oo..XX..",
"     OOX.$ooooooo$oooooo..X.XX.",
"    OXX. $oooooooooooooo.XX....",
"   O ... $oooooooooooooo..O.X..",
"    .O.O $oooooOooooooo.XO..XX.",
"   .   .$$ooo$$O$$ooooo...O.OXX",
"      . $oooooOOooooooo@....OO.",
"        $oooooooooooooooO..XX.O",
"       OoooooooooooooooooO..OX ",
"      OooooooooooooooooooOX OO ",
"      O%ooooooooooooooooooo. O ",
"     O%%%%%%oooooooooooooooO   ",
"     Oo%%%%%%%%%%%%ooooooooO   ",
"     Ooo%%%%%%%%%%%%%%%%%%oO   ",
"     Ooooo%%%%%%%%%%%%%%%%%O   ",
"     Ooooo%%%%%%%%%%%%%ooooO   ",
"     Oooooo%%%%%%%%ooooooooO   ",
"     Oooooo%%%%%%ooooooooooO   ",
"     Oooooooo%%%ooooooooooO    ",
"     OoooooooO+oooooooooooO    ",
"     OoooooooOoooooooooooO     ",
"      OooooooOoooooooooooO     ",
"      OoooooOoooooooooooO      ",
"      OoooooOoooooooooooO      "};


/* XPM */
static char *default_on_xpm[] = {
"31 56 10 1",
" 	c white",
".	c sienna",
"X	c peru",
"o	c wheat",
"O	c black",
"+	c lemon chiffon",
"@	c slate grey",
"#	c red",
"$	c tan",
"%	c magenta",
"             .......XX         ",
"           ..XXXXX...XX        ",
"          ..XX........X.       ",
"         .XXX..oooooo....      ",
"         XX.XoooooooooX .      ",
"         ....oOOOooOOOoX..     ",
"         .X..o O oo O oX...    ",
"         .X.ooO+OooO+OoX.X.    ",
"        ..X.ooo++Oo+++oX.XX.   ",
"        ..X.Xoo+++Oo+oX...X..  ",
"        ..X.X@@oooooo@X...XX.  ",
"      OO.XX..Xoo#OO#oX.XX......",
"   OOO..XX....Xoo##ooX..XXXX...",
"  O....XX...X..XooooXX.....XX..",
"O...............@@@.....oooOXX.",
"    ..O...XXX..o@@...X..OoooO..",
"  O..O...XX...o..XX..X..oooooO.",
"  O.OXX.XX...oo.XX.o.X.$ooooooO",
" O..XX..o..o.o....o..X..ooooooo",
"O.X....o.XXoo..oooo..X.O$oooooo",
"O.OO.Oo....oo.oo+++o.XXXO$ooooo",
" O .OOoo+++oooo+++++o..XOO$$ooo",
"  . O+++++++oO+++++++o..OOO$$oo",
"   O+++++++oO++++%%++++oO$O$$oo",
"   O%%+++++oO+++%##%+++oO$OO$$o",
"  O###+++++oO+o+%OO%++ooO$$..$$",
"  OO%%+++oooOoo++%%+++ooO$$.X.$",
"   OO++oooooOooo+++++ooOO$..X..",
"    OOooooOO$O$ooooooooO$$..XX.",
"     OOOOOO$OO$$$$ooo$OO$$...X.",
"      OOO$$$$$OO$$$$OOO$$$.X.X.",
"      OXO$$ooo$$OOOOO$$$o..X...",
"      OX.$ooooo$$$$$$$$oo..XX..",
"     OOX.$ooooooo$oooooo..X.XX.",
"    OXX. $oooooooooooooo.XX....",
"   O ... $oooooooooooooo..O.X..",
"    .O.O $oooooOooooooo.XO..XX.",
"   .   .$$ooo$$O$$ooooo...O.OXX",
"      . $oooooOOooooooo@....OO.",
"        $oooooooooooooooO..XX.O",
"       OoooooooooooooooooO..OX ",
"      O+oooooooooooooooooOX OO ",
"      Oo++oooooooo+++++++oO. O ",
"     Ooooo++......+$$oooo++O   ",
"     Ooooo$OO....O$$oooooooO   ",
"     Oooooo$$O..OO$ooooooooO   ",
"     Oooooo$$O.OO$$ooooooooO   ",
"     Ooooooo$OOO$$oooooooooO   ",
"     Ooooooo+$O$$+oooooooooO   ",
"     Ooooooo+$O$+ooooooooooO   ",
"     Oooooooo+O+ooooooooooO    ",
"     OoooooooO+oooooooooooO    ",
"     OoooooooOoooooooooooO     ",
"      OooooooOoooooooooooO     ",
"      OoooooOoooooooooooO      ",
"      OoooooOoooooooooooO      "};


/***********************************************************************
 *
 * XpmButton Widget Private Data
 *
 ***********************************************************************/

/************************************
 *
 *  Class structure
 *
 ***********************************/


   /* New fields for the XpmButton widget class record */
typedef struct _XpmButtonClass 
  {
    int makes_compiler_happy;  /* not used */
  } XpmButtonClassPart;

   /* Full class record declaration */
typedef struct _XpmButtonClassRec {
    CoreClassPart		core_class;
#ifdef USE_MOTIF
    XmPrimitiveClassPart  	primitive_class;
#else
    SimpleClassPart		simple_class;
#endif
    XpmButtonClassPart  	xpmbutton_class;
} XpmButtonClassRec;

extern XpmButtonClassRec xpmButtonClassRec;

/***************************************
 *
 *  Instance (widget) structure 
 *
 **************************************/

    /* New fields for the XpmButton widget record */
typedef struct {
    /* resources */
    XtCallbackList 	callback;
    Pixmap		pixmap1;
    Pixmap		pixmap2;
    Pixmap		shape1;
    Pixmap		shape2;
    Pixmap		unsensitive_bitmap;
    XFontStruct		*help_font;
    String		help_string;
    Pixel		help_background;
    Pixel		help_foreground;
    Pixel		shadow_color1;
    Pixel		shadow_color2;
    Dimension		shadow_thickness;

    /* private state */
    GC          	normal_GC;
    GC			color1_GC;
    GC			color2_GC;
    GC			help_GC;
    Pixmap		stipple;
    Window		help_window;
    XtIntervalId	id;
    Boolean     	set;
    Boolean     	help_armed;
} XpmButtonPart;



   /* Full widget declaration */
typedef struct _XpmButtonRec {
    CorePart         core;
#ifdef USE_MOTIF
    XmPrimitivePart  primitive;
#else
    SimplePart	     simple;
#endif
    XpmButtonPart    xpmbutton;
} XpmButtonRec;

#endif /* _XawXpmButtonP_h */






