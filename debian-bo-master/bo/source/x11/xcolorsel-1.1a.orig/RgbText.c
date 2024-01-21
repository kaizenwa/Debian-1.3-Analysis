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

/* WARNING! This code depends from unsigned long being at least 32 bit long!!!!!! */

/* This was inspirated from: */

/* $XConsortium: Template.c,v 1.4 91/02/17 16:18:42 converse Exp $ */

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

#include <stdio.h>
#include <stdlib.h>
/* And here we have a special gift for all SUN's outside >;-> */
#include <unistd.h>
#include <malloc.h>
#include <ctype.h>
#include <errno.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Paned.h>
#include "RgbTextP.h"
#include "RgbSinkP.h"
#include "RgbSrc.h"

#ifndef max
#define	max(a,b)	(((a)>(b))?(a):(b))
#endif

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

/* The maximal length of all ascii representation of color values in all color spaces:*/

#define MAXCOLORLEN 40

format_table formats[]={
	{"rgb",			XcmsRGB16},
	{"rgb4",		XcmsRGB4},
	{"rgb8",		XcmsRGB8},
	{"rgb12",		XcmsRGB12},
	{"rgb16",		XcmsRGB16},
	{"#rgb",		Xcmshash4},
	{"#rrggbb",		Xcmshash8},
	{"#rrrgggbbb",		Xcmshash12},
	{"#rrrrggggbbbb",	Xcmshash16},
	{"input",		Xcmsinput},
	{"ciexyz",		XcmsCIEXYZFormat},
	{"cieuvy",		XcmsCIEuvYFormat},
	{"ciexyy",		XcmsCIExyYFormat},
	{"cielab",		XcmsCIELabFormat},
	{"cieluv",		XcmsCIELuvFormat},
	{"tekhvc",		XcmsTekHVCFormat},
	{"rgbi",		XcmsRGBiFormat},
	{NULL,			XcmsUndefinedFormat},
	};

#define THIRDPART +(~0UL/3UL)	/* Maximum number an unsigned long can hold divided by 3 */

Cursor	defaultcursor=None;

static XtResource resources[] = {
#define offset(field) XtOffsetOf(RgbTextRec, rgb_text.field)
/* {name, class, type, size, offset, default_type, default_addr}, */
{ XtNmaxRows, XtCMaxRows, XtRDimension, sizeof(Dimension), offset(max_rows),
		XtRString, (XtPointer) "30" },
{ XtNrows, XtCRows, XtRDimension, sizeof(Dimension), offset(rows),
		XtRString, (XtPointer) "10" },
{ XtNcolumns, XtCColumns, XtRDimension, sizeof(Dimension), offset(columns),
		XtRString, (XtPointer) "30" },
{ XtNfile, XtCFile, XtRString, sizeof(String), offset(file),
		XtRString, (XtPointer) "/usr/lib/X11/rgb.txt" },
{ XtNtileWidth, XtCTileWidth, XtRDimension, sizeof(Dimension), offset(tile_width),
		XtRString, (XtPointer) "3" },
{ XtNmaxRed, XtCMaxRed, XtRShort, sizeof(unsigned short), offset(max_red),
		XtRString, (XtPointer) "0" },
{ XtNmaxGreen, XtCMaxGreen, XtRShort, sizeof(unsigned short), offset(max_green),
		XtRString, (XtPointer) "0" },
{ XtNmaxBlue, XtCMaxBlue, XtRShort, sizeof(unsigned short), offset(max_blue),
		XtRString, (XtPointer) "0" },
{ XtNmaxIntensity, XtCMaxIntensity, XtRShort, sizeof(unsigned short), offset(max_intensity),
		XtRString, (XtPointer) "255" },
{ XtNbreakPercentage, XtCBreakPercentage, XtRShort, sizeof(unsigned short), offset(break_percentage),
		XtRString, (XtPointer) "9500" },
{ XtNgrabCursor, XtCCursor, XtRCursor, sizeof(Cursor), offset(grabcursor), XtRCursor,
		&defaultcursor },
{ XtNdisplayFormat, XtCDisplayFormat, XtRString, sizeof(String), offset(display_format),
		XtRString, (XtPointer) "input" },
{ XtNrealized, XtCRealized, XtRBoolean, sizeof(Boolean), offset(realized), XtRString,
		(XtPointer) "True"},
{ XtNcomma, XtCComma, XtRBoolean, sizeof(Boolean), offset(comma), XtRString,
		(XtPointer) "False"},
#undef offset
};

static unsigned long abssqr(unsigned short a,unsigned short b);
static void checknhang(Widget w,colordef *owncolor,colordef *colorli);
static void EventHandler(Widget w, XtPointer dummy, XEvent *event, Boolean *cont);
static void class_part_init(WidgetClass wc);
static void init(Widget request, Widget new, ArgList args, Cardinal *num_args);
static String color_name(Widget w);
static int color_spec(Widget w,XColor *color);
static void find_color(Widget w, XColor *color, RgbTextAnswer *answer);
static void grab_color(Widget w, RgbTextAnswerProc answer);
static Boolean set_values(Widget current, Widget request, Widget new,
				 ArgList arg, Cardinal *numargs);
static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *request,
				 XtWidgetGeometry *preferred);
static void set_values_almost(Widget old, Widget new, XtWidgetGeometry *request,
				 XtWidgetGeometry *reply);
static void destroy(Widget w);
static void destroyfile(Widget w);
static Boolean getfile(Widget current, Widget new);
static void printfile(Widget current, Widget new, Boolean force);
static int cmp_cols(colordef *a,colordef *b);
static colordef *get_selection(Widget w);
static colordef *get_cdef(Widget w, XawTextPosition pos);
static int print_color(Widget w, String  destination, colordef *color, Boolean packed);
static int c_needed(unsigned int value);


RgbTextClassRec rgbTextClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &textClassRec,
    /* class_name		*/	"RgbText",
    /* widget_size		*/	sizeof(RgbTextRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	class_part_init,
    /* class_inited		*/	FALSE,
    /* initialize		*/	init,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	TRUE,
    /* destroy			*/	destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	XtInheritExpose,
    /* set_values		*/	set_values,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	set_values_almost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	XtInheritAcceptFocus,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	XtInheritTranslations,
    /* query_geometry		*/	query_geometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
  { /* text fields */
    /* empty			*/	0
  },
  { /* rgb_text fields */
    /* get_color_name		*/	color_name,
    /* get_color_spec		*/	color_spec,
    /* find_color		*/	find_color,
    /* grab_color		*/	grab_color,
    /* print_color		*/	print_color,
  }
};

WidgetClass rgbTextWidgetClass = (WidgetClass)&rgbTextClassRec;

#define myself(widget)          (*(RgbTextRec *)(widget))
#define mysink(widget)          (*(RgbSinkRec *)(*(RgbTextRec *)(widget)).text.sink)
#define myprint(w,d,c,p)	((*(((RgbTextClassRec *)XtClass(w))->rgb_text_class.print_color))(w,d,c,p))

static void class_part_init(WidgetClass wc)
{
register RgbTextClassRec *classptr,*loopptr;

((RgbTextClassRec *)wc)->core_class.compress_motion=
	((TextClassRec *)(rgbTextClassRec.core_class.superclass))->core_class.compress_motion;
((RgbTextClassRec *)wc)->core_class.compress_exposure=
	((TextClassRec *)(rgbTextClassRec.core_class.superclass))->core_class.compress_exposure;
((RgbTextClassRec *)wc)->core_class.compress_enterleave=
	((TextClassRec *)(rgbTextClassRec.core_class.superclass))->core_class.compress_enterleave;
((RgbTextClassRec *)wc)->core_class.visible_interest=
	((TextClassRec *)(rgbTextClassRec.core_class.superclass))->core_class.visible_interest;

/* Enter inheriting here: */

classptr=(RgbTextClassRec *)wc;

loopptr=classptr;
while(loopptr->rgb_text_class.get_color_name==RgbTextInheritGetColorName)
	{
	loopptr=(RgbTextClassRec *)classptr->core_class.superclass;
	}
classptr->rgb_text_class.get_color_name=loopptr->rgb_text_class.get_color_name;	

loopptr=classptr;
while(loopptr->rgb_text_class.get_color_spec==RgbTextInheritGetColorSpec)
	{
	loopptr=(RgbTextClassRec *)classptr->core_class.superclass;
	}
classptr->rgb_text_class.get_color_spec=loopptr->rgb_text_class.get_color_spec;	

loopptr=classptr;
while(loopptr->rgb_text_class.find_color==RgbTextInheritFindColor)
	{
	loopptr=(RgbTextClassRec *)classptr->core_class.superclass;
	}
classptr->rgb_text_class.find_color=loopptr->rgb_text_class.find_color;	

loopptr=classptr;
while(loopptr->rgb_text_class.grab_color==RgbTextInheritGrabColor)
	{
	loopptr=(RgbTextClassRec *)classptr->core_class.superclass;
	}
classptr->rgb_text_class.grab_color=loopptr->rgb_text_class.grab_color;	
}

String GetColorName(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetColorName on non RgbTextWidget\n");
return (*((RgbTextClassRec *)XtClass(w))->rgb_text_class.get_color_name)(w);
}

int GetColorSpec(Widget w, XColor *color)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetColorSpec on non RgbTextWidget\n");
return (*((RgbTextClassRec *)XtClass(w))->rgb_text_class.get_color_spec)(w,color);
}

static Boolean getfile(Widget current, Widget new)
{
unsigned short mred,mgreen,mblue;
long retval;
register size_t i,j;
register char *ptr,*ptn;
Cardinal dummy;
size_t origsize;
char *tailptr;
FILE *fd;

if( (myself(current).rgb_text.file==myself(new).rgb_text.file) &&
	(myself(new).rgb_text.fileisin==True) )
	return False;

mred=myself(new).rgb_text.max_red;
mgreen=myself(new).rgb_text.max_green;
mblue=myself(new).rgb_text.max_blue;
mred=max(mred,myself(new).rgb_text.max_intensity);
mblue=max(mblue,myself(new).rgb_text.max_intensity);
mgreen=max(mgreen,myself(new).rgb_text.max_intensity);

destroyfile(new);
myself(new).rgb_text.fileisin=True;
if(!(fd=fopen(((RgbTextRec *)new)->rgb_text.file,"r")))
        {
	dummy=1;
	XtErrorMsg("nofile","nofile","Fileio","Cannot open RgbTxtFile \"%s\".",
		&(((RgbTextRec *)new)->rgb_text.file),&dummy);
        }
/* find out the size of the file */
if(fseek(fd,0L,SEEK_END))
        {
        seker:
	dummy=0;
	XtErrorMsg("seeker","seeker","Fileio","Seek error on RgbTxtFile.",
		&(((RgbTextRec *)new)->rgb_text.file),&dummy);
        }
retval=ftell(fd);
if(retval<0)
	{
	dummy=0;
	XtErrorMsg("teller","seeker","Fileio","Cannot tell RgbTxtFile position.",
		&(((RgbTextRec *)new)->rgb_text.file),&dummy);
	}
if(fseek(fd,0L,SEEK_SET))
        goto seker;
origsize=retval;
origsize++; /* Just for convenience */
if(!(((RgbTextRec *)new)->rgb_text.origfile=malloc(origsize)))
        {
        nomem:
        dummy=1;
        XtErrorMsg("nomem","nomem","Memory","Not enough memory to scan \"%s\".",
                &(((RgbTextRec *)new)->rgb_text.file),&dummy);
        }
if(1!=fread(((RgbTextRec *)new)->rgb_text.origfile,origsize-1,1L,fd))
	{
	dummy=1;
        XtErrorMsg("reader","reader","Fileio","Read error on \"%s\".",
                &(((RgbTextRec *)new)->rgb_text.file),&dummy);
	}
*(((RgbTextRec *)new)->rgb_text.origfile+origsize-1)='\n';
fclose(fd);
/*Pass one, count input lines */
((RgbTextRec *)new)->rgb_text.numcols=0;
ptr=((RgbTextRec *)new)->rgb_text.origfile;
while(ptr<(((RgbTextRec *)new)->rgb_text.origfile+origsize-1))
        {
        /*skip leading whitespace, watch EOF! */
        while(isspace(*ptr))
                {
                ptr++;
                if(ptr>=(((RgbTextRec *)new)->rgb_text.origfile+origsize-1))
                        goto breakout;
                }
        if(isdigit(*ptr))((RgbTextRec *)new)->rgb_text.numcols++;
        ptn=strchr(ptr,'\n');
        if(!ptn){
                ptn=strchr(ptr,0);
                while(!*ptn)ptn++;
                }
        ptr=ptn;
        }
breakout:
/*Pass two, setup colordef array*/
if(!(((RgbTextRec *)new)->rgb_text.colorlist=malloc((((RgbTextRec *)new)->rgb_text.numcols+1)*sizeof(colordef))))
	goto nomem;
i=0;
ptr=((RgbTextRec *)new)->rgb_text.origfile;
((RgbTextRec *)new)->rgb_text.asciisize=0;
while(i<((RgbTextRec *)new)->rgb_text.numcols)
        {
        /*skip leading whitespace */
        while(!*ptr)ptr++;
        while(isspace(*ptr))ptr++;
        if(isdigit(*ptr))
                {
                /*Accept this line as colordef*/
                retval=strtol(ptr,&tailptr,0);
                if((tailptr==ptr)||(retval<0)||(retval>255))
                        {
                        invalid:
                                ((RgbTextRec *)new)->rgb_text.numcols--;
                                goto skiptoend;
                        }
                ((RgbTextRec *)new)->rgb_text.colorlist[i].red=
	                ((RgbTextRec *)new)->rgb_text.colorlist[i].o_red=retval;
		mred=max(mred,((RgbTextRec *)new)->rgb_text.colorlist[i].o_red);
                ptr=tailptr;
                retval=strtol(ptr,&tailptr,0);
                if((tailptr==ptr)||(retval<0)||(retval>255))
                        goto invalid;
                ((RgbTextRec *)new)->rgb_text.colorlist[i].green=
	                ((RgbTextRec *)new)->rgb_text.colorlist[i].o_green=retval;
		mgreen=max(mgreen,((RgbTextRec *)new)->rgb_text.colorlist[i].o_green);
                ptr=tailptr;
                retval=strtol(ptr,&tailptr,0);
                if((tailptr==ptr)||(retval<0)||(retval>255))
                        goto invalid;
                ((RgbTextRec *)new)->rgb_text.colorlist[i].blue=
	                ((RgbTextRec *)new)->rgb_text.colorlist[i].o_blue=retval;
		mblue=max(mblue,((RgbTextRec *)new)->rgb_text.colorlist[i].o_blue);
                ptr=tailptr;
                while((*ptr!='\n')&&isspace(*ptr))ptr++;
                ((RgbTextRec *)new)->rgb_text.colorlist[i].colorname=ptr;
                ptn=strchr(ptr,'\n');
                if(ptn)*ptn=0;
                ((RgbTextRec *)new)->rgb_text.asciisize+=strlen(ptr);
                ptr=ptn;
                i++;
                continue;
                }
        skiptoend:
        ptn=strchr(ptr,'\n');
        if(!ptn)
                ptn=strchr(ptr,0);
        ptr=ptn;
        }
myself(new).rgb_text.scale_red=(65535L/mred);
myself(new).rgb_text.scale_green=(65535L/mgreen);
myself(new).rgb_text.scale_blue=(65535L/mblue);
myself(new).rgb_text.c_red=c_needed(mred);
myself(new).rgb_text.c_green=c_needed(mgreen);
myself(new).rgb_text.c_blue=c_needed(mblue);
if(!myself(new).rgb_text.ro_colors)
	{
	/* Find colors in a r/w colormap and arrange pixelvalues.. */
	for(i=0;i<myself(new).rgb_text.numcols;i+=myself(new).rgb_text.colsalloc)
		{
		for(j=0;(j<myself(new).rgb_text.colsalloc) && (i+j<myself(new).rgb_text.numcols);j++)
			{
			myself(new).rgb_text.colorlist[i+j].flags=0;
			myself(new).rgb_text.colorlist[i+j].pixtouse=j;
			myself(new).rgb_text.xcolors[j].red=myself(new).rgb_text.colorlist[i+j].red
				*=myself(new).rgb_text.scale_red;;
			myself(new).rgb_text.xcolors[j].green=myself(new).rgb_text.colorlist[i+j].green
				*=myself(new).rgb_text.scale_green;
			myself(new).rgb_text.xcolors[j].blue=myself(new).rgb_text.colorlist[i+j].blue
				*=myself(new).rgb_text.scale_blue;
			}
		XStoreColors(XtDisplay(new),
                	myself(new).core.colormap,
                	myself(new).rgb_text.xcolors,myself(new).rgb_text.colsalloc);
        	XQueryColors(XtDisplay(new),
                	myself(new).core.colormap,
                	myself(new).rgb_text.xcolors,myself(new).rgb_text.colsalloc);
		for(j=0;(j<myself(new).rgb_text.colsalloc) && (i+j<myself(new).rgb_text.numcols);j++)
			{
			myself(new).rgb_text.colorlist[i+j].r_red=myself(new).rgb_text.xcolors[j].red;
			myself(new).rgb_text.colorlist[i+j].r_blue=myself(new).rgb_text.xcolors[j].blue;
			myself(new).rgb_text.colorlist[i+j].r_green=myself(new).rgb_text.xcolors[j].green;
			}
		}
	}
else	{
	/* Find colors in a r/o colormap and arrange pixelvalues.. */
	XColor xcolor;
	
	xcolor.flags=DoRed|DoGreen|DoBlue;

	for(i=0;i<myself(new).rgb_text.numcols;i++)
		{
		myself(new).rgb_text.colorlist[i].flags=0;
		xcolor.red=myself(new).rgb_text.colorlist[i].red
				*=myself(new).rgb_text.scale_red;;
		xcolor.green=myself(new).rgb_text.colorlist[i].green
				*=myself(new).rgb_text.scale_green;
		xcolor.blue=myself(new).rgb_text.colorlist[i].blue
				*=myself(new).rgb_text.scale_blue;
		if(!XAllocColor(XtDisplay(new),myself(new).core.colormap,&xcolor))
			{
			Cardinal dummy;

			dummy=0;
	        	XtErrorMsg("alloc","nocol","ColorAlloc","Could not alloc a readonly color.",
	                	NULL,&dummy);
			}
		myself(new).rgb_text.colorlist[i].r_red=xcolor.red;
		myself(new).rgb_text.colorlist[i].r_blue=xcolor.blue;
		myself(new).rgb_text.colorlist[i].r_green=xcolor.green;
		myself(new).rgb_text.colorlist[i].pixtouse=xcolor.pixel;
		}
				
	}
for(i=0;i<myself(new).rgb_text.numcols;i++)
	{
	if(myself(new).rgb_text.colorlist[i].flags)
		continue;
	for(j=i+1;j<myself(new).rgb_text.numcols;j++)
		{
		myself(new).rgb_text.colorlist[j].flags|=
				cmp_cols(myself(new).rgb_text.colorlist+i,
						myself(new).rgb_text.colorlist+j);
		}
	}
myself(new).rgb_text.diffcols=myself(new).rgb_text.numcols;
myself(new).rgb_text.realcols=myself(new).rgb_text.numcols;
for(i=0;i<myself(new).rgb_text.numcols;i++)
        {
        if(myself(new).rgb_text.colorlist[i].flags&1)
		myself(new).rgb_text.diffcols--;
        if(myself(new).rgb_text.colorlist[i].flags&2)
		myself(new).rgb_text.realcols--;
	}
return True;
}

static void printfile(Widget current, Widget new, Boolean force)
{
long retval;
register size_t i,j;
register char *ptr,*ptn;
Cardinal dummy;
size_t mysize;
char *tailptr;
FILE *fd;
Arg largs[1];

/* Pass three, build ascii repr */
if( myself(new).rgb_text.fileisin != True )
	return;
if( ( myself(current).rgb_text.tile_width == myself(new).rgb_text.tile_width ) &&
	(myself(current).rgb_text.colformat == myself(new).rgb_text.colformat) &&
	(myself(current).rgb_text.realized == myself(new).rgb_text.realized) &&
	(myself(current).rgb_text.comma == myself(new).rgb_text.comma) &&
	( force == False ) )
		return;
myself(new).rgb_text.lasthead=~0L; /* force RgbSink to refetch pointers */
retval=myprint(new,NULL,NULL,False);
((RgbTextRec *)new)->rgb_text.endofrgb=((RgbTextRec *)new)->rgb_text.tile_width+retval+1;

mysize=((RgbTextRec *)new)->rgb_text.asciisize+((RgbTextRec *)new)->rgb_text.numcols*
				(retval+5L+((RgbTextRec *)new)->rgb_text.tile_width)+1;
if(!(((RgbTextRec *)new)->rgb_text.asciirep=malloc(mysize)))
        {
        dummy=0;
        XtErrorMsg("nomem","nomem","Memory","Not enough memory to scan RgbTxtFile.",
                &(((RgbTextRec *)new)->rgb_text.file),&dummy);
        }
ptr=((RgbTextRec *)new)->rgb_text.asciirep;

/* Ensure empty string when numcols == 0 */
*ptr=0;

for(i=0;i<((RgbTextRec *)new)->rgb_text.numcols;i++)
        {
        ((RgbTextRec *)new)->rgb_text.colorlist[i].startofline=ptr;
	for(j=0;j<=((RgbTextRec *)new)->rgb_text.tile_width;j++)
		*ptr++ = ' ';
	ptr+=myprint(new,ptr,((RgbTextRec *)new)->rgb_text.colorlist+i,False);
	sprintf(ptr," - %s\n",((RgbTextRec *)new)->rgb_text.colorlist[i].colorname);
        ptr=strchr(ptr,0);
        }
if(myself(new).rgb_text.numcols && (myself(new).rgb_text.comma!=False))
	{
	ptr=myself(new).rgb_text.colorlist[i-1].startofline;
	tailptr=strchr(ptr,':');
	if(tailptr)
		((RgbTextRec *)new)->rgb_text.startofrgb=(tailptr-ptr)+2;
	else	((RgbTextRec *)new)->rgb_text.startofrgb=((RgbTextRec *)new)->rgb_text.tile_width+1;
	}
else	myself(new).rgb_text.startofrgb=myself(new).rgb_text.tile_width+1;
((RgbTextRec *)new)->rgb_text.colorlist[i].startofline=ptr;
XtSetArg(largs[0], XtNstring, ((RgbTextRec *)new)->rgb_text.asciirep );
XtSetValues(new, largs, 1);
}

static void init(Widget request, Widget new, ArgList args, Cardinal *num_args)
{
Widget sink,source;
Arg largs[10];
ArgList newargs;
Cardinal dummy=1;
int idummy=0;
XGCValues GCdummy;
XVisualInfo vis_info, *vis_ptr;
unsigned long *pixvals;
register size_t i;
format_table *form_ptr;


((RgbTextRec *)new)->rgb_text.asciirep="";
((RgbTextRec *)new)->rgb_text.actual=NULL;
((RgbTextRec *)new)->rgb_text.first=NULL;
((RgbTextRec *)new)->rgb_text.fileisin=False;
((RgbTextRec *)new)->rgb_text.actual=NULL;
((RgbTextRec *)new)->rgb_text.numhits=0;
((RgbTextRec *)new)->rgb_text.numexact=0;
((RgbTextRec *)new)->rgb_text.repres=XtMalloc((size_t)(MAXCOLORLEN+1));
((RgbTextRec *)new)->rgb_text.grab_callback=NULL;

/* Set colorformat: */
myself(new).rgb_text.colformat=Xcmsinput;
for(form_ptr=formats;(form_ptr->format_name); form_ptr++)
	{
	if(!strcasecmp(form_ptr->format_name,myself(new).rgb_text.display_format))
		{
		myself(new).rgb_text.colformat=form_ptr->format;
		break;
		}
	}

XtInsertEventHandler(new, ButtonPressMask|ButtonReleaseMask|PointerMotionMask, False, 
			EventHandler, NULL, XtListHead);
XtSetArg (largs[0], XtNstring, ((RgbTextRec *)new)->rgb_text.asciirep);
XtSetArg (largs[1], XtNuseStringInPlace, True);
XtSetArg (largs[2], XtNeditType, XawtextRead);
newargs=XtMergeArgLists(args,*num_args,largs,3);

source=XtCreateWidget("source",rgbSrcObjectClass,new,newargs,*num_args+3);
sink=XtCreateWidget("sink",rgbSinkObjectClass,new,newargs,*num_args+3);

/* Idiotic prototype of XtFree(char *) requires casting: */
XtFree((void *)newargs);

((RgbTextRec *)new)->rgb_text.colorgc=XtAllocateGC(
        new,0,0,&GCdummy,GCForeground,
                GCLineWidth|GCLineStyle|GCFillRule|GCFont|GCGraphicsExposures|
                GCDashOffset|GCDashList|GCArcMode);

myself(new).rgb_text.myCCC=XcmsCCCOfColormap(XtDisplay(new),myself(new).core.colormap);

vis_info.visualid=XVisualIDFromVisual(VisualOfCCC(myself(new).rgb_text.myCCC));
vis_ptr=XGetVisualInfo(XtDisplay(new),VisualIDMask,&vis_info,&idummy);
if(!vis_ptr)
	{
	char tmpc[20],*tmpptr=tmpc;

	sprintf(tmpc,"0x%lx",vis_info.visualid);
       	dummy=1;

	XtErrorMsg("novisual","nocol","ColorAlloc","Cannot get visual info for ID %s.",
		&tmpptr,&dummy);
	}
switch(vis_ptr->class)
	{
	case PseudoColor:
	case GrayScale:
	case DirectColor:
		((RgbTextRec *)new)->rgb_text.ro_colors=False;
		break;
	case StaticColor:
	case StaticGray:
	case TrueColor:
		((RgbTextRec *)new)->rgb_text.ro_colors=True;
		break;
	default:
		{
		char tmpc[20],*tmpptr=tmpc;

		sprintf(tmpc,"0x%x",vis_ptr->class);
        	dummy=1;
		
        	XtErrorMsg("visual","nocol","ColorAlloc","Unknown/Unsupported Visual %s.",
                	&tmpptr,&dummy);
		}
	}

if(!((RgbTextRec *)new)->rgb_text.ro_colors)
	{
	((RgbTextRec *)new)->rgb_text.colsalloc=((RgbTextRec *)new)->rgb_text.max_rows;
	pixvals=malloc(((size_t)(((RgbTextRec *)new)->rgb_text.colsalloc))*sizeof(unsigned long));
	((RgbTextRec *)new)->rgb_text.xcolors=
		malloc(((size_t)(((RgbTextRec *)new)->rgb_text.colsalloc))*sizeof(XColor));
	if((!pixvals)||(!((RgbTextRec *)new)->rgb_text.xcolors))
		{
        	dummy=0;
        	XtErrorMsg("nomemcol","nomem","ColorAlloc","Not enough memory for colortable.\nReduce maxRows!.",
                	&(((RgbTextRec *)request)->rgb_text.file),&dummy);
		}
	for(;;)	{
		if(XAllocColorCells(XtDisplay(new),
			((RgbTextRec *)new)->core.colormap,FALSE,
        		NULL,0,pixvals,((RgbTextRec *)new)->rgb_text.colsalloc))
				break;
		if(((RgbTextRec *)new)->rgb_text.colsalloc==1)
                	{
	        	dummy=0;
	        	XtErrorMsg("nocol","nocol","ColorAlloc","Cannot allocate any color.",
	                	&(((RgbTextRec *)request)->rgb_text.file),&dummy);
			}
		((RgbTextRec *)new)->rgb_text.colsalloc--;
		}
	for(i=0;i<((RgbTextRec *)new)->rgb_text.colsalloc;i++)
		{
		((RgbTextRec *)new)->rgb_text.xcolors[i].flags=DoRed|DoGreen|DoBlue;
		((RgbTextRec *)new)->rgb_text.xcolors[i].pixel=pixvals[i];
		}
	if(((RgbTextRec *)new)->rgb_text.max_rows!=((RgbTextRec *)new)->rgb_text.colsalloc)
		{
		char tmpc[2][20];
        	char *(tmpptr)[3];
        	tmpptr[0]=tmpc[0];
        	tmpptr[1]=tmpc[0];
        	tmpptr[2]=tmpc[1];
		sprintf(tmpc[0],"%lu",(unsigned long)((RgbTextRec *)new)->rgb_text.colsalloc);
		sprintf(tmpc[1],"%lu",(unsigned long)((RgbTextRec *)new)->rgb_text.max_rows);
		dummy=3;
		XtWarningMsg("somecol","somecol","ColorAlloc",
			"Warning: Only %s colors available.\n"
			"Setting maxRows to %s instead of %s!",
			tmpptr,&dummy);
		}
	((RgbTextRec *)new)->rgb_text.max_rows=((RgbTextRec *)new)->rgb_text.colsalloc;
	free(pixvals);
	}
else	myself(new).rgb_text.colsalloc=0;

XtSetArg (largs[0], XtNtextSink, sink);
XtSetArg (largs[1], XtNtextSource, source);
XtSetValues(new, largs, 2);
XtSetArg (largs[0], XtNwidth, myself(new).rgb_text.preferred_width);
XtSetArg (largs[1], XtNheight, myself(new).rgb_text.preferred_height);
XtSetValues(new, largs, 2);
}

static void destroy(Widget w)
{
XtReleaseGC(w,((RgbTextRec *)w)->rgb_text.colorgc);
XcmsFreeCCC(myself(w).rgb_text.myCCC);
free(((RgbTextRec *)w)->rgb_text.xcolors);
destroyfile(w);
XtRemoveEventHandler(w, ButtonPressMask|ButtonReleaseMask|PointerMotionMask, False, 
			EventHandler, NULL);
}

static void destroyfile(Widget w)
{
if(myself(w).rgb_text.fileisin==True)
	{
	if(((RgbTextRec *)w)->rgb_text.asciirep)
		free(((RgbTextRec *)w)->rgb_text.asciirep);
	free(((RgbTextRec *)w)->rgb_text.origfile);

	if(myself(w).rgb_text.ro_colors)
		{
		int i;

		for(i=0;i<myself(w).rgb_text.numcols;i++)
			{
			XFreeColors(XtDisplay(w),myself(w).core.colormap,
				&myself(w).rgb_text.colorlist[i].pixtouse,
				1,0);
			}
		}

	free(((RgbTextRec *)w)->rgb_text.colorlist);
	((RgbTextRec *)w)->rgb_text.asciirep=NULL;
	myself(w).rgb_text.fileisin=False;
	}
}

static String color_name(Widget w)
{
colordef *ptr;

ptr=get_selection(w);
if(!ptr)
	return NULL;
return ptr->colorname;
}

static int color_spec(Widget w,XColor *color)
{
colordef *ptr;

color->pixel=0;
color->flags=DoRed|DoGreen|DoBlue;
ptr=get_selection(w);
if(!ptr)
	{
	color->red=color->green=color->blue=0;
	return 0;
	}
color->red=ptr->red;
color->green=ptr->green;
color->blue=ptr->blue;
return 0;
}

static Boolean set_values(Widget current, Widget request, Widget new,
				 ArgList arg, Cardinal *numargs)
{
unsigned int lineheight,cellwidth;
format_table *form_ptr;
Arg largs[1];

if(myself(new).rgb_text.max_intensity<1)
	myself(new).rgb_text.max_intensity=1;
if(myself(new).rgb_text.break_percentage>=10000)
	{
	myself(new).rgb_text.break_percentage=10000;
	myself(new).rgb_text.break_value=0;
	}
else	{
	if(myself(new).rgb_text.break_percentage<=3333)
		myself(new).rgb_text.break_percentage=3334;
	myself(new).rgb_text.break_value=
		(10000L-myself(new).rgb_text.break_percentage)*0x65535L+5000L;
	myself(new).rgb_text.break_value/=10000;
	myself(new).rgb_text.break_value*=0x10001UL;
	}

#ifdef LIB_X11R6
lineheight=mysink(new).ascii_sink.font->ascent+mysink(new).ascii_sink.font->descent;
cellwidth=XTextWidth(mysink(new).ascii_sink.font," ",1);
#else
lineheight=mysink(new).text_sink.font->ascent+mysink(new).text_sink.font->descent;
cellwidth=XTextWidth(mysink(new).text_sink.font," ",1);
#endif

if(myself(new).rgb_text.colsalloc)
	{
	if(myself(new).rgb_text.colsalloc<myself(new).rgb_text.max_rows)
		myself(new).rgb_text.max_rows=myself(new).rgb_text.colsalloc;
	}
if(myself(new).rgb_text.max_rows<myself(new).rgb_text.rows)
	myself(new).rgb_text.rows=myself(new).rgb_text.max_rows;

myself(new).rgb_text.preferred_width=myself(new).text.margin.left+
					myself(new).text.margin.right+
					myself(new).rgb_text.columns*cellwidth;
myself(new).rgb_text.preferred_height=myself(new).text.margin.top+
					myself(new).text.margin.bottom+
					myself(new).rgb_text.rows*lineheight;
myself(new).rgb_text.max_pixels=myself(new).text.margin.top+
					myself(new).text.margin.bottom+
					myself(new).rgb_text.max_rows*(Dimension)lineheight;
if(myself(new).rgb_text.max_pixels!=myself(current).rgb_text.max_pixels)
	{
	XtSetArg (largs[0], XtNmax, myself(new).rgb_text.max_pixels);
	XtSetValues(new, largs, 1);
	}	
myself(new).rgb_text.colformat=Xcmsinput;
for(form_ptr=formats;(form_ptr->format_name); form_ptr++)
	{
	if(!strcasecmp(form_ptr->format_name,myself(new).rgb_text.display_format))
		{
		myself(new).rgb_text.colformat=form_ptr->format;
		break;
		}
	}
printfile(current, new, getfile(current, new));
return False;
}

static XtGeometryResult query_geometry(Widget w, XtWidgetGeometry *request,
				 XtWidgetGeometry *preferred)
{
XtGeometryMask requested;
XtGeometryResult result=XtGeometryYes;

if((myself(w).core.height==myself(w).rgb_text.preferred_height)&&
	(myself(w).core.width==myself(w).rgb_text.preferred_width))
		return XtGeometryNo;
if(request->request_mode&CWWidth)
	{
	if(request->width!=myself(w).rgb_text.preferred_width)
		preferred->width=myself(w).rgb_text.preferred_width;
	result=XtGeometryAlmost;
	}	
if(request->request_mode&CWHeight)
	{
	if(request->height!=myself(w).rgb_text.preferred_height)
		preferred->height=myself(w).rgb_text.preferred_height;
	result=XtGeometryAlmost;
	}
preferred->request_mode=request->request_mode&(CWWidth|CWHeight);
return result;
}

static void set_values_almost(Widget old, Widget new, XtWidgetGeometry *request,
				 XtWidgetGeometry *reply)
{
reply=request;
if(request->request_mode&CWHeight)
	{
	if(request->height>myself(new).rgb_text.max_pixels)
		reply->height=myself(new).rgb_text.max_pixels;
	}
}

String GetRgbFile(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetRgbFile on non RgbTextWidget\n");

return myself(w).rgb_text.file;
}

Cardinal GetNumEntries(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetNumEntries on non RgbTextWidget\n");
if( myself(w).rgb_text.fileisin != True )
	return 0;

return myself(w).rgb_text.numcols;
}

Cardinal GetNumDifferent(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetNumDifferent on non RgbTextWidget\n");
if( myself(w).rgb_text.fileisin != True )
	return 0;

return myself(w).rgb_text.diffcols;
}

Cardinal GetNumRealisable(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetNumRealisable on non RgbTextWidget\n");
if( myself(w).rgb_text.fileisin != True )
	return 0;

return myself(w).rgb_text.realcols;
}

Dimension GetMaxPixels(Widget w)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GetMaxPixels on non RgbTextWidget\n");

return myself(w).rgb_text.max_pixels;
}

static int cmp_cols(colordef *a,colordef *b)
{
int retval=0;

if((a->red==b->red)&&(a->green==b->green)&&(a->blue==b->blue)) retval|=1;
if((a->r_red==b->r_red)&&(a->r_green==b->r_green)&&(a->r_blue==b->r_blue)) retval|=2;
return retval;
}

static colordef *get_cdef(Widget w, XawTextPosition pos)
{
char * liptr;
size_t first,last,new;

liptr=myself(w).rgb_text.asciirep+pos;
first=0;
last=myself(w).rgb_text.numcols-1;
while(last-first>1)
        {
        new=(first+last)/2;
        if(myself(w).rgb_text.colorlist[new].startofline<=liptr)
                first=new;
        else    last=new;
        }
if(myself(w).rgb_text.colorlist[last].startofline<=liptr)
        new=last;
else    new=first;
return myself(w).rgb_text.colorlist+new;
}

static colordef *get_selection(Widget w)
{
XawTextPosition begin,end;
XawTextGetSelectionPos(w,&begin,&end);
if(begin==end)
	{
	if(myself(w).text.display_caret!=True)
		return NULL;
	begin=XawTextGetInsertionPoint(w);
	}
return get_cdef(w,begin);
}

void FindColor(Widget w, XColor *color, RgbTextAnswer *answer)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply FindColor on non RgbTextWidget\n");
(*((RgbTextClassRec *)XtClass(w))->rgb_text_class.find_color)(w,color,answer);
}

void GrabColor(Widget w, RgbTextAnswerProc answer_proc)
{
XtCheckSubclass(w, rgbTextWidgetClass, "Cannot apply GrabColor on non RgbTextWidget\n");
(*((RgbTextClassRec *)XtClass(w))->rgb_text_class.grab_color)(w,answer_proc);
}


static void find_color(Widget w, XColor *color, RgbTextAnswer *answer)
{
colordef owncolor;

answer->repres=myself(w).rgb_text.repres;
if(color)
	{
	register size_t i;
	colordef *colorli;

	myself(w).rgb_text.numhits=0;
	myself(w).rgb_text.numexact=0;
	myself(w).rgb_text.first=NULL;
	owncolor.red=owncolor.r_red=owncolor.o_red=color->red;
	owncolor.green=owncolor.r_green=owncolor.o_green=color->green;
	owncolor.blue=owncolor.r_blue=owncolor.o_blue=color->blue;
	myprint(w,answer->repres,&owncolor,True);
	/* do the search here */
	for(i=0,colorli=myself(w).rgb_text.colorlist;
			i<myself(w).rgb_text.numcols;
			i++,colorli++)
		checknhang(w,&owncolor,colorli);
	colorli=myself(w).rgb_text.first;
	myself(w).rgb_text.numhits=0;
	while(colorli)
		{
		colorli->flags+=0x10001L/2;
		colorli->flags/=0x10001L;
		colorli->flags*=10000;
		colorli->flags+=0xffffL/2;
		colorli->flags/=0xffffL;
		colorli->flags=10000-colorli->flags;
		if(colorli->flags>=10000L)
			{
			if((owncolor.red==colorli->r_red)
				&&(owncolor.green==colorli->r_green)
				&&(owncolor.blue==colorli->r_blue))
					colorli->flags=10000L;
			else	colorli->flags=9999L;
			}
		if(colorli->flags<myself(w).rgb_text.break_percentage)
			{
			/* break list here */
			if(colorli->prev)
				colorli->prev->next=NULL;
			else	myself(w).rgb_text.first=NULL;
			break;
			}
		myself(w).rgb_text.numhits++;
		colorli=colorli->next;
		}
	if(myself(w).rgb_text.first)
		{
		answer->offset=1;
		myself(w).rgb_text.actual=myself(w).rgb_text.first;
		}
	else	{
		answer->offset=0;
		}
	}
else	{
	if(answer->offset>=myself(w).rgb_text.numhits)
		answer->offset=myself(w).rgb_text.numhits;
	/* Now find the line: */
	if(!answer->offset)
		{
		; /* do nothing, could jump also */
		}
	else if(answer->offset>myself(w).rgb_text.offset)
		{
		/*Go on from actual:*/
		go_on_actual:
		while(answer->offset>myself(w).rgb_text.offset)
			{
			myself(w).rgb_text.actual=myself(w).rgb_text.actual->next;
			myself(w).rgb_text.offset++;
			}
		}
	else if((myself(w).rgb_text.offset-answer->offset)<answer->offset)
		{
		/* Is nearer to actual than to start of list, go backwards.. */
		while(answer->offset<myself(w).rgb_text.offset)
			{
			myself(w).rgb_text.actual=myself(w).rgb_text.actual->prev;
			myself(w).rgb_text.offset--;
			}
		}
	else	{
		/*search from start*/
		myself(w).rgb_text.actual=myself(w).rgb_text.first;
		myself(w).rgb_text.offset=1;
		goto go_on_actual;
		}
	}
myself(w).rgb_text.offset=answer->offset;
if(answer->offset)
	{
	register char *ptr;
	register XawTextPosition start,end;

	answer->percentage=myself(w).rgb_text.actual->flags;
	ptr=myself(w).rgb_text.actual->startofline;
	start=(ptr-myself(w).rgb_text.asciirep);
	start+=1+myself(w).rgb_text.tile_width;
	end=(strchr(ptr,'\n')-myself(w).rgb_text.asciirep);
	/*Display selection:*/
	XawTextSetSelection(w, start, end);
	XawTextSetInsertionPoint(w, end);
	/*Set cursor*/
	}
else	{
	/* deselect any selection */
	XawTextSetSelection(w, 0l, 0l);
	answer->percentage=0;
	}
			
answer->numhits=myself(w).rgb_text.numhits;
answer->numexact=myself(w).rgb_text.numexact;
}

static void grab_color(Widget w, RgbTextAnswerProc answer)
{
XColor mycolor;

/* Grab pointer here: */
myself(w).rgb_text.grabtime=XtLastTimestampProcessed(XtDisplay(w));

#ifdef GRABSERVER
XGrabServer(XtDisplay(w));
#endif

#ifdef NOCOLORMAPINSTALL
#define GRABEVENTS (ButtonPressMask|ButtonReleaseMask)
#else
#define GRABEVENTS (ButtonPressMask|ButtonReleaseMask|PointerMotionMask)
#endif

XtGrabPointer(w, False, GRABEVENTS, GrabModeAsync, GrabModeAsync,
		None, myself(w).rgb_text.grabcursor, myself(w).rgb_text.grabtime);
#undef GRABEVENTS
myself(w).rgb_text.grab_callback=answer;
myself(w).rgb_text.grab_catched=False;
/* save actual installed colormaps: */
#ifndef NOCOLORMAPINSTALL
myself(w).rgb_text.saved_maps=XListInstalledColormaps(XtDisplay(w),XtWindow(w),
				&(myself(w).rgb_text.saved_colormaps));
#else
myself(w).rgb_text.saved_maps=0;
#endif
}

static void EventHandler(Widget w, XtPointer dummy, XEvent *event, Boolean *cont)
{
#ifdef REINSTALLCOLORMAPS
int i;
#endif

/* Grab in process? */
if(!(myself(w).rgb_text.grab_callback))
	return;
/* Is button already grabbed ? */
if( ((XButtonEvent *)event)->time < myself(w).rgb_text.grabtime )
	return;

#ifndef NOCOLORMAPINSTALL
if(event->type == MotionNotify)
	{
	int destx,desty;
	Window destw,sourcew,subw;
	XWindowAttributes attrib;

	/* Now find the window that was entered: */
	destw=subw=((XMotionEvent *)event)->root;
	destx=((XMotionEvent *)event)->x_root;
	desty=((XMotionEvent *)event)->y_root;
	do	{
		sourcew=destw;
		destw=subw;
		XTranslateCoordinates(((XMotionEvent *)event)->display,sourcew,destw,
			destx,desty,
			&destx, &desty, &subw);
		}
	while(subw!=None);
	/* Get its colormap: */
	XGetWindowAttributes(((XMotionEvent *)event)->display,destw,&attrib);
	/* and install it... */	
	XInstallColormap(((XMotionEvent *)event)->display,attrib.colormap);
	}
else
#endif /* Yes this is not nice.. but most efficient. */
    if(event->type == ButtonRelease)
	{
	if( ((XButtonEvent *)event)->button != Button1 )
		{
		*cont=False;
		return; /* Ignore it */
		}
	if(myself(w).rgb_text.grab_catched!=False)
		{
		/* End grab: */
		myself(w).rgb_text.grab_callback=NULL;
		XtUngrabPointer(w, ((XButtonEvent *)event)->time);
#ifndef NOCOLORMAPINSTALL
#ifdef REINSTALLCOLORMAPS
		for(i=myself(w).rgb_text.saved_colormaps-1;i>=0;i--)
			XInstallColormap(((XButtonEvent *)event)->display,
				myself(w).rgb_text.saved_maps[i]);
#endif
#endif
		XFree(myself(w).rgb_text.saved_maps);

#ifdef GRABSERVER
		XUngrabServer(XtDisplay(w));
#endif
		}
	}
else if(event->type == ButtonPress)
	{
	int destx,desty;
	Window destw,sourcew,subw;
	XImage *image;
	XWindowAttributes attrib;

	if( ((XButtonEvent *)event)->button != Button1 )
		{
		/* actively reject event: */
		error:
		XBell(XtDisplay(w), 50);
		*cont=False;
		return;
		}
	/* The grab was catched:*/
	myself(w).rgb_text.grab_catched=True;

	/* Now find the color value: */
	/* First the window the pointer was in: */
	destw=subw=((XButtonEvent *)event)->root;
	destx=((XButtonEvent *)event)->x_root;
	desty=((XButtonEvent *)event)->y_root;
	do	{
		sourcew=destw;
		destw=subw;
		XTranslateCoordinates(((XButtonEvent *)event)->display,sourcew,destw,
			destx,desty,
			&destx, &desty, &subw);
		}
	while(subw!=None);
	/* Get the pixel value under pointer: */
	image=XGetImage(((XButtonEvent *)event)->display,destw,destx,desty,1,1,~0L,XYPixmap);
	if(!image)
		goto error;
	myself(w).rgb_text.grabcol.pixel=XGetPixel(image,0,0);
	XDestroyImage(image);
	/* Get the colormap: */
	XGetWindowAttributes(((XButtonEvent *)event)->display,destw,&attrib);
	/* Look up the color */	
	XQueryColor(XtDisplay(w),attrib.colormap,&(myself(w).rgb_text.grabcol));

	FindColor(w, &(myself(w).rgb_text.grabcol), &(myself(w).rgb_text.answer));
	myself(w).rgb_text.grabcol.flags=DoRed|DoGreen|DoBlue;
	(*(myself(w).rgb_text.grab_callback))
		(w, &(myself(w).rgb_text.grabcol), &(myself(w).rgb_text.answer));
	}

*cont=False;
return;
}

static int c_needed(unsigned int value)
{
if(value>9999)
	return 5;
else if(value>999)
	return 4;
else if(value>99)
	return 3;
else if(value>9)
	return 2;
return 1;
}

static int print_color(Widget w, String  destination, colordef *color, Boolean packed)
{
char *format,*comma=", ";
int fieldsize,stringsize;
XcmsColor cmscolin;

/* for Lab,Luv,HVC */
if(packed==True)
	fieldsize=1;
else	fieldsize=6;
/* Prepare cmscolin: */
cmscolin.format=XcmsRGBFormat;
if(myself(w).rgb_text.comma==False)
	comma="/";
if(color)
	{
	if(myself(w).rgb_text.realized!=False)
		{
		cmscolin.spec.RGB.red=color->r_red;
		cmscolin.spec.RGB.green=color->r_green;
		cmscolin.spec.RGB.blue=color->r_blue;
		}
	else	{
		cmscolin.spec.RGB.red=color->red;
		cmscolin.spec.RGB.green=color->green;
		cmscolin.spec.RGB.blue=color->blue;
		}
	}
switch(myself(w).rgb_text.colformat)
	{
	case XcmsRGB4:
		cmscolin.spec.RGB.red=(cmscolin.spec.RGB.red*0xfUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.green=(cmscolin.spec.RGB.green*0xfUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.blue=(cmscolin.spec.RGB.blue*0xfUL+0x7fffUL)/0xffffUL;
		fieldsize=1;
		goto prin_rgb;
	case XcmsRGB8:
		cmscolin.spec.RGB.red=(cmscolin.spec.RGB.red*0xffUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.green=(cmscolin.spec.RGB.green*0xffUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.blue=(cmscolin.spec.RGB.blue*0xffUL+0x7fffUL)/0xffffUL;
		fieldsize=2;
		goto prin_rgb;
	case XcmsRGB12:
		cmscolin.spec.RGB.red=(cmscolin.spec.RGB.red*0xfffUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.green=(cmscolin.spec.RGB.green*0xfffUL+0x7fffUL)/0xffffUL;
		cmscolin.spec.RGB.blue=(cmscolin.spec.RGB.blue*0xfffUL+0x7fffUL)/0xffffUL;
		fieldsize=3;
		goto prin_rgb;
	case XcmsRGB16:
		fieldsize=4;
		prin_rgb:
		if(myself(w).rgb_text.comma!=False)
			{
			format="rgb: %#*x%s%#*x%s%#*x";
			fieldsize+=2;
			}
		else
			format="rgb: %*x%s%*x%s%*x";
		stringsize=7+3*fieldsize;
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(packed==True)
			fieldsize=1;
		if(destination)
			sprintf(destination,format,fieldsize,cmscolin.spec.RGB.red,comma,
				fieldsize,cmscolin.spec.RGB.green,comma,fieldsize,cmscolin.spec.RGB.blue);
		break;
	case Xcmshash4:
		cmscolin.spec.RGB.red>>=12;
		cmscolin.spec.RGB.green>>=12;
		cmscolin.spec.RGB.blue>>=12;
		format="#%01x%01x%01x";
		stringsize=4;
		goto prin_hash;
	case Xcmshash8:
		cmscolin.spec.RGB.red>>=8;
		cmscolin.spec.RGB.green>>=8;
		cmscolin.spec.RGB.blue>>=8;
		format="#%02x%02x%02x";
		stringsize=7;
		goto prin_hash;
	case Xcmshash12:
		cmscolin.spec.RGB.red>>=4;
		cmscolin.spec.RGB.green>>=4;
		cmscolin.spec.RGB.blue>>=4;
		format="#%03x%03x%03x";
		stringsize=10;
		goto prin_hash;
	case Xcmshash16:
		format="#%04x%04x%04x";
		stringsize=13;
		prin_hash:
		if(destination)
			sprintf(destination,format,cmscolin.spec.RGB.red,
				cmscolin.spec.RGB.green,cmscolin.spec.RGB.blue);
		break;
	default:
	case Xcmsinput:
		stringsize=myself(w).rgb_text.c_red+
				myself(w).rgb_text.c_green+
				myself(w).rgb_text.c_blue+4+7;
		if(destination)
			{
			if(packed==False)
			    sprintf(destination,"input: %*d, %*d, %*d",
				myself(w).rgb_text.c_red,color->o_red,
				myself(w).rgb_text.c_green,color->o_green,
				myself(w).rgb_text.c_blue,color->o_blue);
			else
			    sprintf(destination,"%d, %d, %d",
				color->o_red,
				color->o_green,
				color->o_blue);
			}
		break;
	case XcmsCIEXYZFormat:
		stringsize=8+6+1+6+1+6;
		format="CIEXYZ: %6.4f%s%6.4f%s%6.4f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="CIEXYZ: "; 
					converr:
					strcpy(destination,format);
					fieldsize=strlen(destination);
					format=strchr(format,0);
					while(fieldsize<stringsize)
						{
						*format++ = '-';
						fieldsize++;
						}
					*format=0;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.CIEXYZ.X,comma,
					cmscolin.spec.CIEXYZ.Y,comma,
					cmscolin.spec.CIEXYZ.Z);
				}
			}
		break;
	case XcmsCIEuvYFormat:
		stringsize=8+6+1+6+1+6;
		format="CIEuvY: %6.4f%s%6.4f%s%6.4f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="CIEuvY: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.CIEuvY.u_prime,comma,
					cmscolin.spec.CIEuvY.v_prime,comma,
					cmscolin.spec.CIEuvY.Y);
				}
			}
		break;
		break;
	case XcmsCIExyYFormat:
		stringsize=8+6+1+6+1+6;
		format="CIExyY: %6.4f%s%6.4f%s%6.4f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="CIExyY: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.CIExyY.x,comma,
					cmscolin.spec.CIExyY.y,comma,
					cmscolin.spec.CIExyY.Y);
				}
			}
		break;
	case XcmsCIELabFormat:
		stringsize=8+6+1+6+1+6;
		format="CIELab: %6.2f%s%6.3f%s%6.3f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="CIELab: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.CIELab.L_star,comma,
					cmscolin.spec.CIELab.a_star,comma,
					cmscolin.spec.CIELab.b_star);
				}
			}
		break;
	case XcmsCIELuvFormat:
		stringsize=8+6+1+6+1+6;
		format="CIELuv: %6.2f%s%6.3f%s%6.3f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="CIELuv: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.CIELuv.L_star,comma,
					cmscolin.spec.CIELuv.u_star,comma,
					cmscolin.spec.CIELuv.v_star);
				}
			}
		break;
	case XcmsTekHVCFormat:
		stringsize=8+6+1+6+1+6;
		format="TekHVC: %6.2f%s%6.2f%s%6.2f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="TekHVC: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.TekHVC.H,comma,
					cmscolin.spec.TekHVC.V,comma,
					cmscolin.spec.TekHVC.C);
				}
			}
		break;
	case XcmsRGBiFormat:
		stringsize=6+6+1+6+1+6;
		format="rgbi: %6.4f%s%6.4f%s%6.4f";
		if(myself(w).rgb_text.comma!=False)
			{
			stringsize+=2;
			}
		if(destination)
			{
			if(XcmsConvertColors(myself(w).rgb_text.myCCC,&cmscolin,1,myself(w).rgb_text.colformat,NULL)
				==XcmsFailure)
					{
					format="rgbi: "; 
					goto converr;
					}
			else	{
				sprintf(destination,format,
					cmscolin.spec.RGBi.red,comma,
					cmscolin.spec.RGBi.green,comma,
					cmscolin.spec.RGBi.blue);
				}
			}
		break;
	}
if(destination)
	{
	return strlen(destination);
	}
return stringsize;
}

static unsigned long abssqr(unsigned short a,unsigned short b)
{
unsigned long tmp;
tmp=labs(((long)a)-((long)b));
tmp*=tmp;
if(tmp>THIRDPART)
	tmp=THIRDPART;
return tmp;
}

static void checknhang(Widget w,colordef *owncolor,colordef *colorli)
{
unsigned long diff;
colordef *cptr;

diff =abssqr(owncolor->r_red,colorli->r_red);
diff+=abssqr(owncolor->r_green,colorli->r_green);
diff+=abssqr(owncolor->r_blue,colorli->r_blue);
if((diff<=myself(w).rgb_text.break_value)&&(diff<=3*THIRDPART))
	{
	myself(w).rgb_text.numhits++;
	if((owncolor->r_red==colorli->r_red)&&
		(owncolor->r_green==colorli->r_green)&&
		(owncolor->r_blue==colorli->r_blue))
		{
		myself(w).rgb_text.numexact++;
		colorli->flags=0;
		}
	else	{
		colorli->flags=diff?diff:1; 
		}
	/* sortin */
	if(!myself(w).rgb_text.first)
		{
		/*create list with one element*/
		myself(w).rgb_text.first=colorli;
		colorli->prev=NULL;
		colorli->next=NULL;
		}
	else	{
		for(cptr=myself(w).rgb_text.first;;cptr=cptr->next)
			{
			/* color is less good? */
			if(cptr->flags>diff)
				{
				/* insert before */
				colorli->next=cptr;
				colorli->prev=cptr->prev;
				if(cptr->prev)
					{
					(cptr->prev)->next=colorli;
					}
				else	myself(w).rgb_text.first=colorli;		
				cptr->prev=colorli;
				return;
				}
			/*There will be no next/aka append*/
			if(!(cptr->next))
				{
				cptr->next=colorli;
				colorli->next=NULL;
				colorli->prev=cptr;
				return;
				}
			}
		}
	}
}
