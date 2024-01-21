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

/******************************************************************************************
* This defines an RgbSink Object. This is a subclass of an AsciiSink. It is designed to     *
* to be used by an RgbText Object. The only difference to an AsciiSink is a special scan   *
* function.                                                                               *
******************************************************************************************/

/* This was inspirated from: */


/* $XConsortium: Template.c,v 1.4 91/02/17 16:18:42 converse Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "RgbSinkP.h"

/* Find current Xrelease number */
#ifndef LIB_X11R6
#ifndef LIB_X11R5
#if XlibSpecificationRelease == 6
#define LIB_X11R6
#else
#if XlibSpecificationRelease == 5
#define LIB_X11R5
#else
#error Could not determine which Xlib is used, define LIB_X11R5 or LIB_X11R6.
#endif /* XLibSpecificationRelease==5 */
#endif /* XLibSpecificationRelease==6 */
#endif /* LIB_X11R5 */
#endif /* LIB_X11R6 */

/* No resources */

/* No actions */

/* Both are in RgbText! */

/* No actions no translations */

static void initialize(Widget request, Widget new, ArgList args, Cardinal *numargs);
static void mydisp(Widget w, Position x, Position y, XawTextPosition pos1,
		   XawTextPosition pos2, Boolean highlight);

RgbSinkClassRec rgbSinkClassRec = {
  { /* object fields */
    /* ======        */ 
    /* superclass		*/	(WidgetClass) &asciiSinkClassRec,
    /* class_name		*/	"RgbSink",
    /* widget_size		*/	sizeof(RgbSinkRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	NULL,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	NULL,
    /* num_resources		*/	0,
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	0,
    /* compress_exposure	*/	0,
    /* compress_enterleave	*/	0,
    /* visible_interest		*/	0,
    /* destroy			*/	NULL,
    /* resize			*/	NULL,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	NULL,
    /* display_accelerator	*/	NULL,
    /* extension		*/	NULL
  },
  { /* text_sink fields */
    /* DisplayText		*/	(_XawSinkVoidFunc) mydisp,
    /* InsertCursor		*/	XtInheritInsertCursor,
    /* ClearToBackground	*/	XtInheritClearToBackground,
    /* FindPosition		*/	XtInheritFindPosition,
    /* FindDistance		*/	XtInheritFindDistance,
    /* Resolve			*/	XtInheritResolve,
    /* MaxLines			*/	XtInheritMaxLines,
    /* MaxHeight		*/	XtInheritMaxHeight,
    /* SetTabs			*/	XtInheritSetTabs,
    /* GetCursorBounds		*/	XtInheritGetCursorBounds,
  },
  { /* ascii_sink fields */
    /* empty			*/	0
  },
  { /* rgb_sink fields */
    /* empty			*/	0
  }
};

WidgetClass rgbSinkObjectClass = (WidgetClass)&rgbSinkClassRec;

#define rgbparent(widget)	(*(RgbTextRec *)XtParent(widget))
#define myself(widget)		(*(RgbSinkRec *)(widget))

static void initialize(Widget request, Widget new, ArgList args, Cardinal *numargs)
{
XtCheckSubclass(XtParent(new),rgbTextWidgetClass,
"rgbSink: Didn't I say that my parent has to be a subclass of RgbText?\n");
rgbparent(new).rgb_text.lasthead=~0l;
}

static void mydisp(Widget w, Position x, Position y, XawTextPosition pos1,
		   XawTextPosition pos2, Boolean highlight)
{
int tile,linum;
char *liptr;
size_t first,last,new;
XawTextPosition fipos;
unsigned int cellwidth,tileheight;

(*((AsciiSinkClassRec *)(rgbSinkClassRec.object_class.superclass))->text_sink_class.DisplayText)
		(w, x, y, pos1, pos2, highlight);

/* Find last headline.. actually not really needed with r/o colors */
if(rgbparent(w).rgb_text.lasthead!=XawTextTopPosition(XtParent(w)))
	{
	register size_t i,des;
	rgbparent(w).rgb_text.lasthead=XawTextTopPosition(XtParent(w));
	liptr=rgbparent(w).rgb_text.asciirep+rgbparent(w).rgb_text.lasthead;
	first=0;
	last=rgbparent(w).rgb_text.numcols;
	if(last)
		last--;
	while(last-first>1)
		{
		new=(first+last)/2;
		if(rgbparent(w).rgb_text.colorlist[new].startofline<=liptr)
			first=new;
		else	last=new;
		}
	if(rgbparent(w).rgb_text.colorlist[last].startofline<=liptr)
		new=last;
	else	new=first;
	myself(w).rgb_sink.headline=rgbparent(w).rgb_text.colorlist+new;

	if(!(rgbparent(w).rgb_text.ro_colors))
		{
		/* Store actual color values in to the Server */
		for(i=0;((i+new)<rgbparent(w).rgb_text.numcols) &&
				(i<rgbparent(w).rgb_text.colsalloc);i++)
			{
			des=myself(w).rgb_sink.headline[i].pixtouse;
			rgbparent(w).rgb_text.xcolors[des].red=myself(w).rgb_sink.headline[i].red;
			rgbparent(w).rgb_text.xcolors[des].green=myself(w).rgb_sink.headline[i].green;
			rgbparent(w).rgb_text.xcolors[des].blue=myself(w).rgb_sink.headline[i].blue;
			}
		XStoreColors(XtDisplay(XtParent(w)),
			rgbparent(w).core.colormap,
			rgbparent(w).rgb_text.xcolors,rgbparent(w).rgb_text.colsalloc);
		}
	/* Nothing needs to be done for R/O cells */
	}

#ifdef LIB_X11R6
tileheight=myself(w).ascii_sink.font->ascent+myself(w).ascii_sink.font->descent;
cellwidth=XTextWidth(myself(w).ascii_sink.font," ",1);
#else
tileheight=myself(w).text_sink.font->ascent+myself(w).text_sink.font->descent;
cellwidth=XTextWidth(myself(w).text_sink.font," ",1);
#endif

linum=(y-rgbparent(w).text.margin.top)/tileheight;
fipos=myself(w).rgb_sink.headline[linum].startofline-rgbparent(w).rgb_text.asciirep;
fipos+=rgbparent(w).rgb_text.tile_width;
/*printf("DispText(*,%ld,%ld,%ld,%ld,*)%ld,%ld\n",(long)x,(long)y,(long)pos1,(long)pos2,
	(long)linum,(long)fipos);*/
if(pos1<fipos)
	{
	if(rgbparent(w).rgb_text.ro_colors)
		{ /* directly use pixtouse */
		XSetForeground(XDisplayOfScreen(w->core.parent->core.screen),
			rgbparent(w).rgb_text.colorgc,
			myself(w).rgb_sink.headline[linum].pixtouse);
		}
	else	{ /* pixtouse is just an index into xcolors */
		XSetForeground(XDisplayOfScreen(w->core.parent->core.screen),
			rgbparent(w).rgb_text.colorgc,
			rgbparent(w).rgb_text.xcolors[myself(w).rgb_sink.headline[linum].pixtouse].pixel
			);
		}
	XFillRectangle(XDisplayOfScreen(w->core.parent->core.screen),
		w->core.parent->core.window,rgbparent(w).rgb_text.colorgc,x,y,
			(unsigned int)(cellwidth*(fipos-pos1)),(tileheight-1));
	}
}
