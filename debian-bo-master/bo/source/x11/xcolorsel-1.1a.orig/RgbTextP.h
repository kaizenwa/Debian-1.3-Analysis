/*********************************************************************************/
/* This file is  part of Xcolorsel, a multipurpose viewer for X11 rgb.txt files. */
/*                     Copyright (C) 1993, 1994 Michael Weller                   */
/*                                                                               */
/* This program is free software; you can redistribute it and/or modify it under */
/* the terms of the GNU General Public License as published be the Free Software */
/* Foundation;  either version 2 of the License,  or (at your opinion) any later */
/* version.                                                                      */
/*                                                                               */
/* This program is  distributed in the hope  that it will be useful, but WITHOUT */
/* ANY WARRANTY; without even the  implied warranty of  MERCHANBILITY or FITNESS */
/* FOR  A  PARTICULAR  PURPOSE.   See the  GNU General  Public License  for more */
/* details.                                                                      */
/*                                                                               */
/* You should have received a copy  of the GNU General Public License along with */
/* this program; if not,  write to the  Free Software Foundation, Inc., 675 Mass */
/* Ave, Cambridge, MA 02139, USA.                                                */
/*                                                                               */
/* The author can be  reached by means of email  as eowmob@exp-math.uni-essen.de */
/* or  eowmob@pollux.exp-math.uni-essen.de   or   mat42b@vm.hrz.uni-essen.de  or */
/* mat42b@de0hrz1a.bitnet.  Or as:  Michael Weller,  Heiderhoefen 116b,  D 46049 */
/* Oberhausen, Germany.                                                          */
/*                                                                               */
/* Part of the files are derived  from the Template Widget of the Athena Widgets */
/* as published  by the  Massachusetts Institute of  Technology.  Actually these */
/* files do not contain any code  by the M.I.T  but only  variable declarations. */
/* Nevertheless these parts of these files are still distributed under the terms */
/* of the M.I.T copyright which are here repeated:                               */
/*                                                                               */
/* Copyright    Massachusetts Institute of Technology   1987, 1988               */
/*                                                                               */
/* Permission to use, copy,  modify, distribute,  and sell this software and its */
/* documentation for  any purpose is hereby  granted without fee,  provided that */
/* the  above  copyright  notice  appear  in  all  copies  and  that  both  that */
/* copyright   notice   and   this  permission   notice  appear  in   supporting */
/* documentation,  and that the  name of M.I.T.  not  be used  in advertising or */
/* publicity  pertaining  to distribution  of  the  software  without  specific, */
/* written  prior  permission.    M.I.T.  makes  no  representations  about  the */
/* suitability  of  this  software  for  any  purpose.   It is provided  "as is" */
/* without express or implied warranty.                                          */
/*                                                                               */
/* M.I.T. DISCLAIMS ALL  WARRANTIES WITH REGARD  TO THIS SOFTWARE, INCLUDING ALL */
/* IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS,  IN NO EVENT SHALL M.I.T. */
/* BE LIABLE FOR ANY SPECIAL,  INDIRECT OR CONSEQUENTIAL  DAMAGES OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE,  DATA OR PROFITS, WHETHER IN AN ACTION */
/* OF CONTRACT,  NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,  ARISING OUT OF  OR IN */
/* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                      */
/*********************************************************************************/

/* This was inspirated from: */

/* $XConsortium: TemplateP.h,v 1.6 91/03/13 20:12:07 rws Exp $ */

/* Copyright    Massachusetts Institute of Technology   1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef _RgbTextP_h
#define _RgbTextP_h

#include "RgbText.h"
#include "config.h"

#include <X11/Xaw/TextP.h>
#include <X11/Xcms.h>

typedef struct _colordef { unsigned short red, green, blue; /*Theoretic RGB*/
		 unsigned short r_red, r_green, r_blue; /* Realized on Screen */
		 unsigned short o_red, o_green, o_blue; /* Read in (non scaled) */
		 unsigned long flags;
                 char *startofline;
		 char *colorname;
                 unsigned long pixtouse;
		 struct _colordef *prev,*next;
                } colordef;

/* Table of displayFormats : */
typedef struct  {
	char *format_name;
	XcmsColorFormat format;
	}
	format_table;

#define XcmsRGB4        0x10000000UL
#define XcmsRGB8        0x10000001UL
#define XcmsRGB12       0x10000002UL
#define XcmsRGB16       0x10000003UL
#define Xcmshash4       0x10000004UL
#define Xcmshash8       0x10000005UL
#define Xcmshash12      0x10000006UL
#define Xcmshash16      0x10000007UL
#define Xcmsinput       0x10000008UL


typedef String (*RgbTextGetColorNameProc)(
	Widget w
	);

typedef int (*RgbTextGetColorSpecProc)(
	Widget w,
	XColor *color
	);

typedef void (*RgbTextFindColorProc)(
	Widget w,
	XColor *color,
	RgbTextAnswer *answer
	);

typedef void (*RgbTextGrabColorProc)(
	Widget w,
	RgbTextAnswerProc answer_proc
	);

typedef int (*RgbTextPrintColorProc)(
	Widget w,
	String	destination,
	colordef *color,
	Boolean packed
	);

#define RgbTextInheritGetColorName ((RgbTextGetColorNameProc)_XtInherit)
#define RgbTextInheritGetColorSpec ((RgbTextGetColorSpecProc)_XtInherit)
#define RgbTextInheritFindColor ((RgbTextFindColorProc)_XtInherit)
#define RgbTextInheritGrabColor ((RgbTextGrabColorProc)_XtInherit)
#define RgbTextInheritPrintColor ((RgbTextPrintColorProc)_XtInherit)

typedef struct	{
		RgbTextGetColorNameProc	get_color_name;
		RgbTextGetColorSpecProc	get_color_spec;
		RgbTextFindColorProc	find_color;
		RgbTextGrabColorProc	grab_color;
		RgbTextPrintColorProc	print_color;
		} RgbTextClassPart;

typedef struct _RgbTextClassRec
	{
	CoreClassPart	core_class;
	SimpleClassPart	simple_class;
	TextClassPart	text_class;
	RgbTextClassPart	rgb_text_class;
	} RgbTextClassRec;

extern RgbTextClassRec rgbTextClassRec;

typedef struct	{
		/* resources */
		Dimension	max_rows;
		Dimension	rows;
		Dimension	columns;
		Dimension	tile_width;
		String		file;
		String		display_format;
		unsigned short	max_red;
		unsigned short	max_green;
		unsigned short	max_blue;
		unsigned short  max_intensity;
		unsigned short	break_percentage;
		Boolean		realized;
		Boolean		comma;
	
		/* private state */
		Boolean		fileisin;
		Boolean		grab_catched;
		Boolean		ro_colors;
		unsigned short	scale_red;
		unsigned short	scale_green;
		unsigned short	scale_blue;
		unsigned short	c_red;
		unsigned short	c_green;
		unsigned short	c_blue;
		Dimension	max_pixels;
		Dimension	preferred_width;
		Dimension	preferred_height;
		Dimension	startofrgb;
		Dimension	endofrgb;
		int		saved_colormaps;
		Cardinal	asciisize;
		Cardinal	numcols;
		Cardinal	diffcols;
		Cardinal	realcols;
		Cardinal	colsalloc;
		Cardinal	numhits;
		Cardinal	numexact;
		Cardinal	offset;
		unsigned long	break_value;
		XawTextPosition	lasthead;
		XcmsColorFormat	colformat;
		String		asciirep;
		String		origfile;
		String		repres;
		colordef	*colorlist;
		colordef	*actual;
		colordef	*first;
		Time		grabtime;
		XColor		grabcol;
		XColor		*xcolors;
		Cursor		grabcursor;
		GC		colorgc;
		XcmsCCC		myCCC;
		RgbTextAnswer	answer;
		RgbTextAnswerProc
				grab_callback;
		Colormap	*saved_maps;
		} RgbTextPart;

typedef struct _RgbTextRec {
		CorePart	core;
		SimplePart	simple;
		TextPart	text;
		RgbTextPart	rgb_text;
		} RgbTextRec;


#endif /* _RgbTextP_h */
