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

#include <stdio.h>
#include <stdlib.h>
/* And here we have a special gift for all SUN's outside >;-> */
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <malloc.h>
#include <fcntl.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/AsciiText.h>

#include "config.h"
#include "RgbText.h"
#include "defhelp.h"
#include "xcslicon"

#ifdef BIGLENS

/* Warning: we have to deal later with this case too... */
#include "biglens"
#include "biglensMask"

#else /* BIGLENS */

#include "lens"
#include "lensMask"

#endif /* BIGLENS */

#ifndef HELPFILE
/* No default setting*/
#define HELPFILE ""
#endif

#define DEFAULTCLASS "Xcolorsel"

/* Do not edit the following by Hand... it is intended to be used with patch. */
#define VERSION "1.0a"

char *myname;

Widget myshell,pane,box,fileinfo,quitbutton,text,message,commandbox,dispbox,defbutt,comma,realized,
	setback,setfore,reverse,next,prev,grab,best,textform,messageform,menub,menu,help,
	help_close,help_shell=NULL,help_pane,help_form,help_text,help_buttonbox;

typedef struct	{
	Widget sme;
	char *name;
	char *value;
	} entrytype;

entrytype entry[]= {
	{ NULL,	"input", "input"},
	{ NULL,	"rgb4", "rgb4"},
	{ NULL,	"rgb8", "rgb8"},
	{ NULL,	"rgb12", "rgb12"},
	{ NULL,	"rgb16", "rgb16"},
	{ NULL,	"rgbi", "rgbi"},
	{ NULL,	"ciexyz", "ciexyz"},
	{ NULL,	"cieuvy", "cieuvy"},
	{ NULL,	"ciexyy", "ciexyy"},
	{ NULL,	"cielab", "cielab"},
	{ NULL,	"cieluv", "cieluv"},
	{ NULL,	"tekhvc", "tekhvc"},
	{ NULL,	"srgb4", "#rgb"},
	{ NULL,	"srgb8", "#rrggbb"},
	{ NULL,	"srgb12", "#rrrgggbbb"},
	{ NULL,	"srgb16", "#rrrrggggbbbb"}
	};

String MyName,MyClass,headline;

XtAppContext mycontext;
Arg args[10];
Cursor mycursor = None;
Pixel defback,deffore;
RgbTextAnswer answer;
String messbuf=NULL;
size_t messlen=0;

String my_fallbacks[]={
#include "appdef-c.h"
		    NULL, /* Marker, do not remove */
#include "appdef.h"
		    NULL};

String *fallbacks;
XrmOptionDescRec options[]={
		{"-color",	"*.customization", XrmoptionNoArg,	(XtPointer)"-color"},
		{"-nocolor",	"*.customization", XrmoptionNoArg,	(XtPointer)""},
		{"-busycursor",	"*.busyCursor",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-helpfile",	"*.helpfile",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-maxrows",	"*.maxRows",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-rows",	"*.rows",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-columns",	"*.columns",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-tilewidth",	"*.tileWidth",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-file",	"*.file",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-grabcursor",	"*.grabCursor",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-maxred",	"*.maxRed",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-maxgreen",	"*.maxGreen",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-maxblue",	"*.maxBlue",	XrmoptionSepArg,	(XtPointer)NULL},
		{"-maxintensity",	"*.maxIntensity",	XrmoptionSepArg,
									(XtPointer)NULL},
		{"-breakpercentage",	"*.breakPercentage",	XrmoptionSepArg,
									(XtPointer)NULL},
		{"-cstyle",	"*.comma",	XrmoptionNoArg,		(XtPointer)"True"},
		{"-resourcestyle", "*.comma",	XrmoptionNoArg,		(XtPointer)"False"},
		{"-internal",	"*.realized",	XrmoptionNoArg,		(XtPointer)"False"},
		{"-realized",	"*.realized",	XrmoptionNoArg,		(XtPointer)"True"},
		{"-input",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"input"},
		{"-rgb4",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"rgb4"},
		{"-rgb8",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"rgb8"},
		{"-rgb12",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"rgb12"},
		{"-rgb16",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"rgb16"},
		{"-rgbi",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"rgbi"},
		{"-ciexyz",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"ciexyz"},
		{"-cieuvy",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"cieuvy"},
		{"-ciexyy",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"ciexyy"},
		{"-cielab",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"cielab"},
		{"-cieluv",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"cieluv"},
		{"-tekhvc",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"tekhvc"},
		{"-#4",		"*.displayFormat",XrmoptionNoArg,	(XtPointer)"#rgb"},
		{"-#8",		"*.displayFormat",XrmoptionNoArg,	(XtPointer)"#rrggbb"},
		{"-#12",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"#rrrgggbbb"},
		{"-#16",	"*.displayFormat",XrmoptionNoArg,	(XtPointer)"#rrrrggggbbbb"},
		};
typedef struct  {
	String		info_message;
	String		info_default;
	String		info_fore;
	String		info_back;
	String		info_nocol;
	String		info_outof;
	String		info_reverse;
	String		info_grab;
	String		info_grabinit;
	String		info_next;
	String		info_nohit;
	Cursor		busy_cursor;
	String		help_file;
        } appres_type;
appres_type appres;
XtResource appreslist[]={
        {"infoFile","InfoFile",XtRString,sizeof(String),XtOffsetOf(appres_type,info_message),
			XtRString,"       %lu entries: %lu different, %lu different on screen.       "},
        {"infoDefaults","InfoDefaults",XtRString,sizeof(String),XtOffsetOf(appres_type,info_default),
			XtRString,"Fore- and background returned to defaults."},
        {"infoForeground","InfoForeground",XtRString,sizeof(String),XtOffsetOf(appres_type,info_fore),
			XtRString,"Foreground set to `%s'."},
        {"infoBackground","InfoBackground",XtRString,sizeof(String),XtOffsetOf(appres_type,info_back),
			XtRString,"Background set to `%s'."},
        {"infoNoColor","InfoNoColor",XtRString,sizeof(String),XtOffsetOf(appres_type,info_nocol),
			XtRString,"Please select a color out of the list first!"},
        {"infoOutOfColors","InfoNoColor",XtRString,sizeof(String),XtOffsetOf(appres_type,info_outof),
			XtRString,"Sorry, out of colors... Defaults restored."},
        {"infoReverse","InfoReverse",XtRString,sizeof(String),XtOffsetOf(appres_type,info_reverse),
			XtRString,"Fore- and background exchanged."},
        {"infoGrab","InfoGrab",XtRString,sizeof(String),XtOffsetOf(appres_type,info_grab),
			XtRString,"%s: %lu matches (%lu exact). Best %d.%02d%%."},
        {"infoGrabInit","InfoGrabInit",XtRString,sizeof(String),XtOffsetOf(appres_type,info_grabinit),
			XtRString,"Select a color to find with the left mouse button."},
        {"infoNext","InfoNext",XtRString,sizeof(String),XtOffsetOf(appres_type,info_next),
			XtRString,"Match %lu (%d.%02d%%) out of %lu (%lu exact)."},
        {"infoNoHit","InfoNoHit",XtRString,sizeof(String),XtOffsetOf(appres_type,info_nohit),
			XtRString,"%s: No match better %d.%02d%% found."},
        {"busyCursor","BusyCursor",XtRCursor,sizeof(Cursor),XtOffsetOf(appres_type,busy_cursor),
			XtRString,"watch"},
	{"helpfile","Helpfile",XtRString,sizeof(String),XtOffsetOf(appres_type,help_file),
			XtRString, HELPFILE}
	};

Atom wm_delete_window;

void usage(int argc,char *argv[]);
void getfile(void);
void exerr(char *mess);
void quit(Widget w, XtPointer a, XtPointer b);
void wm_protocols(Widget w, XEvent *event , String *params, Cardinal *num_params);
void info(Widget w, XtPointer a, XtPointer b);
void dispsel(Widget w, XtPointer a, XtPointer b);
void show_help(Widget w, XtPointer a, XtPointer b);
void close_help(Widget w, XtPointer a, XtPointer b);
void set_color(Widget w, XtPointer a, XtPointer b);
void grabcolor(Widget w, XtPointer a, XtPointer b);
void nexthit(Widget w, XtPointer a, XtPointer b);
void allocmess(size_t len);

XtActionsRec actions[] = {
	{ "wm_protocols", wm_protocols },
	};

String translations="<Message>WM_PROTOCOLS: wm_protocols()";

void main(int argc,char *argv[])
{
size_t i;
register char *(*ptr);
Dimension maxpixels,actheight;
Pixel curfore, curback;
Boolean state;
Pixmap icon_pixmap = None;

myname=strrchr(argv[0],'/');
if(myname)
	myname++;
else	myname=argv[0];

/*
/* Select normal fallbacks: */
fallbacks= my_fallbacks;
for(i=0;*fallbacks;i++,fallbacks++); /* NOTE: i is needed later!!!! */
fallbacks++;

/* Scan for -color parameter: */
ptr=(argv+1);
while(*ptr)
	{
	if(!strcmp(*ptr,"-color"))
		{
		/* Reinclude color settings and erase the first marking NULL: */
		/* fallbacks points to one behind NULL */
		while(i--)
			{
			fallbacks--;
			/* Get previous elt: */
			*fallbacks = *(fallbacks-1);
			}
		break;
		}
	ptr++;
	}
myshell=XtAppInitialize(&mycontext,DEFAULTCLASS,options,XtNumber(options),&argc,argv,
		fallbacks,NULL,0);
if(argc>2)
	{
	if(argv[1][0]=='-')
		usage(argc,argv);
	usage(argc-1,argv+1);
	}
if(argc==2)
	{
	if(argv[1][0]=='-')
		usage(argc,argv);
	}
wm_delete_window=XInternAtom(XtDisplay(myshell),"WM_DELETE_WINDOW",False);
XtAppAddActions(mycontext,actions,XtNumber(actions));
XtOverrideTranslations(myshell,XtParseTranslationTable(translations));
XtGetApplicationResources(myshell,&appres,appreslist,XtNumber(appreslist),NULL,0);
XtGetApplicationNameAndClass(XtDisplay(myshell),&MyName,&MyClass);

XtSetArg (args[0], XtNiconPixmap, &icon_pixmap);
XtGetValues(myshell, args, 1);
if (icon_pixmap == None)
	{
	XtSetArg(args[0], XtNiconPixmap, 
	XCreateBitmapFromData(XtDisplay(myshell),
	XtScreen(myshell)->root, (char *)xcslicon_bits, xcslicon_width, xcslicon_height));
	XtSetValues (myshell, args, 1);
	}

pane=XtCreateManagedWidget("rootpane",panedWidgetClass,myshell,NULL,0);

XtSetArg (args[0], XtNresizeToPreferred, True);
XtSetArg (args[1], XtNskipAdjust, True);
XtSetArg (args[2], XtNshowGrip, False);
commandbox=XtCreateManagedWidget("commandbox", boxWidgetClass,pane,args,3);

dispbox=XtCreateManagedWidget("dispbox", boxWidgetClass,pane,args,3);

XtSetArg (args[0], XtNshowGrip, False);
textform=XtCreateManagedWidget("textform", formWidgetClass,pane,args,1);

XtSetArg (args[0], XtNfile, argv[1]);
text=XtCreateManagedWidget("text",rgbTextWidgetClass,textform,args,(argc>1)?1:0);
XtSetArg (args[0], XtNfile, &MyClass);
XtSetArg (args[1], XtNforeground, &deffore);
XtSetArg (args[2], XtNbackground, &defback);
XtSetArg (args[3], XtNgrabCursor, &mycursor);
XtSetArg (args[4], XtNpointerColor, &curfore);
XtSetArg (args[5], XtNpointerColorBackground, &curback);
XtGetValues(text, args, 6);

if (mycursor == None)
	{
	Pixmap cursor,mask;
	XColor cfor,cbak;
	XrmValue colorval,pixel;
	
	pixel.addr= (XPointer)&curfore;
	pixel.size= sizeof(Pixel);
	colorval.addr= (XPointer)&cfor;
	colorval.size= sizeof(XColor);
	XtConvertAndStore(text, XtRPixel, &pixel, XtRColor, &colorval);
	
	pixel.addr= (XPointer)&curback;
	pixel.size= sizeof(Pixel);
	colorval.addr= (XPointer)&cbak;
	colorval.size= sizeof(XColor);
	XtConvertAndStore(text, XtRPixel, &pixel, XtRColor, &colorval);

#ifdef BIGLENS
	cursor=XCreateBitmapFromData(XtDisplay(text),XRootWindowOfScreen(XtScreen(text)),
			biglens_bits,biglens_width,biglens_height);
	mask=XCreateBitmapFromData(XtDisplay(text),XRootWindowOfScreen(XtScreen(text)),
			biglensMask_bits,biglensMask_width,biglensMask_height);
	mycursor=XCreatePixmapCursor(XtDisplay(text),cursor,mask,&cfor,&cbak,
			biglens_x_hot,biglens_y_hot);
#else /* BIGLENS */
	cursor=XCreateBitmapFromData(XtDisplay(text),XRootWindowOfScreen(XtScreen(text)),
			lens_bits,lens_width,lens_height);
	mask=XCreateBitmapFromData(XtDisplay(text),XRootWindowOfScreen(XtScreen(text)),
			lensMask_bits,lensMask_width,lensMask_height);
	mycursor=XCreatePixmapCursor(XtDisplay(text),cursor,mask,&cfor,&cbak,lens_x_hot,lens_y_hot);
#endif /* BIGLENS */
	XtSetArg (args[0], XtNgrabCursor, mycursor);
	XtSetValues (text, args, 1);
	XFreePixmap(XtDisplay(text), cursor);
	XFreePixmap(XtDisplay(text), mask);
	}
headline=XtMalloc(strlen(MyClass)+strlen(MyName)+3l);
sprintf(headline,"%s: %s",MyName,MyClass);
XtSetArg (args[0], XtNtitle, headline);
XtSetValues(myshell, args, 1);

XtSetArg (args[0], XtNresizeToPreferred, True);
XtSetArg (args[1], XtNshowGrip, False);
XtSetArg (args[2], XtNskipAdjust, True);
messageform=XtCreateManagedWidget("messageform", formWidgetClass,pane,args,3);

message=XtCreateManagedWidget("message", labelWidgetClass,messageform,args,0);
XtSetArg (args[0], XtNresizeToPreferred, True);
XtSetArg (args[1], XtNskipAdjust, True);
box=XtCreateManagedWidget("buttonbox", boxWidgetClass,pane,args,2);

help=XtCreateManagedWidget("about", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(help,XtNcallback,show_help,NULL);

fileinfo=XtCreateManagedWidget("fileinfo", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(fileinfo,XtNcallback,info,NULL);

grab=XtCreateManagedWidget("grab", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(grab,XtNcallback,grabcolor,NULL);

reverse=XtCreateManagedWidget("reverse", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(reverse,XtNcallback,set_color,NULL);

defbutt=XtCreateManagedWidget("defaults", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(defbutt,XtNcallback,set_color,NULL);

quitbutton=XtCreateManagedWidget("quit", commandWidgetClass,commandbox,NULL,0);
XtAddCallback(quitbutton,XtNcallback,quit,NULL);

XtSetArg (args[0], XtNmenuName, "displayMenu");
menub=XtCreateManagedWidget("display", menuButtonWidgetClass,dispbox,args,1);

menu=XtCreatePopupShell("displayMenu", simpleMenuWidgetClass,menub,NULL,0);

for(i=0;i<(sizeof(entry)/sizeof(entrytype));i++)
	{
	entry[i].sme=XtCreateManagedWidget(entry[i].name, smeBSBObjectClass,menu,NULL,0);
	XtAddCallback(entry[i].sme, XtNcallback, dispsel, NULL);
	}

XtSetArg(args[0], XtNcomma, &state);
XtGetValues(text, args, 1);
XtSetArg(args[0], XtNstate, state);
comma=XtCreateManagedWidget("comma", toggleWidgetClass,dispbox,args,1);
XtAddCallback(comma,XtNcallback,dispsel,NULL);

XtSetArg(args[0], XtNrealized, &state);
XtGetValues(text, args, 1);
XtSetArg(args[0], XtNstate, state);
realized=XtCreateManagedWidget("realized", toggleWidgetClass,dispbox,args,1);
XtAddCallback(realized,XtNcallback,dispsel,NULL);

XtSetArg(args[0],XtNsensitive,False);
best=XtCreateManagedWidget("best", commandWidgetClass,box,args,1);
XtAddCallback(best,XtNcallback,grabcolor,NULL);

XtSetArg(args[0],XtNsensitive,False);
prev=XtCreateManagedWidget("prev", commandWidgetClass,box,args,1);
XtAddCallback(prev,XtNcallback,nexthit,NULL);

XtSetArg(args[0],XtNsensitive,False);
next=XtCreateManagedWidget("next", commandWidgetClass,box,args,1);
XtAddCallback(next,XtNcallback,nexthit,NULL);

setfore=XtCreateManagedWidget("setfore", commandWidgetClass,box,NULL,0);
XtAddCallback(setfore,XtNcallback,set_color,NULL);

setback=XtCreateManagedWidget("setback", commandWidgetClass,box,NULL,0);
XtAddCallback(setback,XtNcallback,set_color,NULL);

info(NULL,NULL,NULL);

XtRealizeWidget(myshell);

XSetWMProtocols(XtDisplay(myshell),XtWindow(myshell),&wm_delete_window,1);

/* Make sure that the maximum height of the rgb_text widget is not exceeded: */
maxpixels=GetMaxPixels(text);
XtSetArg (args[0], XtNheight, &actheight);
XtGetValues(text, args, 1);
maxpixels-=actheight;

XtSetArg (args[0], XtNheight, &actheight);
XtGetValues(textform, args, 1);
actheight+=maxpixels;
XtSetArg (args[0], XtNmax, actheight);
XtSetValues(textform, args, 1);

XtSetArg (args[0], XtNheight, &actheight);
XtGetValues(myshell, args, 1);
actheight+=maxpixels;
XtSetArg (args[0], XtNmaxHeight, actheight);
XtSetValues(myshell, args, 1);

XtAppMainLoop(mycontext);
}

void usage(int argc,char *argv[])
{
int i;
if(argc!=1)
	{
	fprintf(stderr,"%s: Invalid options/additional parameters\nOffending arguments are:\n%s",
		myname,myname);
	for(i=1;i<argc;i++)
		fprintf(stderr," %s",argv[i]);
	fputs("\n",stderr);
	}	
fprintf(stderr,
"usage: %s [options] [RgbTxtFile]\n"
"\tdisplays contents of RgbTxtFile (default is\n"
"\t/usr/lib/X11/rgb.txt) together with color\n"
"\tsamples.\n"
"Allowed options are:\n"
" Toolkit options:\n"
"  -background color\tspecifies the overall background color.\n"
"  -bd color\t\tspecifies the overall border color.\n"
"  -bg color\t\tspecifies the overall background color.\n"
"  -borderwidth n\tspecifies the overall border width in pixels.\n"
"  -bordercolor color\tspecifies the overall border color.\n"
"  -bw n\t\t\tspecifies the overall border width in pixels.\n"
"  -display disp\t\tspecifies Xserver to connect to. (Otherwise $DISPLAY\n"
"\t\t\tis used.)\n"
"  -fg color\t\tspecifies the overall foreground color.\n"
"  -fn font\t\tspecifies the overall text font.\n"
"  -font font\t\tspecifies the overall text font.\n"
"  -foreground color\tspecifies the overall foreground color.\n"
"  -geometry geom\tstartup geometry of the window in standard notation.\n"
"\t\t\tThe size of the geometry is overridden by -rows and -columns.\n"
"  -iconic\t\tstartup as icon.\n"
"  -name name\t\tspecifies name to use for resource determination.\n"
"  -reverse\t\tstartup with fore- and background exchanged.\n"
"  -rv\t\t\tstartup with fore- and background exchanged.\n"
"  +rv\t\t\tstartup without fore- and background exchanged.\n"
"  -selectionTimeout n\tspecifies timeout for selections.\n"
"  -title title\t\tspecifies title of the mainwindow.\n"
"  -synchronous\t\tforces synchronous communication with the Server.\n"
"\t\t\t(for debugging purposes only)\n"
"  +synchronous\t\tforces asynchronous communication with the Server.\n"
"\t\t\t(for debugging purposes only)\n"
"  -xnllanguage lang\tspecifies language to use.\n"
"  -xrm string\t\tallows setting of any resourse with a string like.\n"
"\t\t\t\"Resource: value\" for example:\n"
"\t\t\t\"*.pointerColor: red\".\n"
" Xcolorsel options:\n"
"  -color\t\tsets *.customization: -color, so forcing the use of\n"
"\t\t\tcolor app-defaults.\n"
"  -nocolor\t\tsets *.customization: \"\", so forcing the use of\n"
"\t\t\tblack&white app-defaults.\n"
"  -maxrows n\t\tmaximum number of lines to display at once.\n"
"\t\t\texactly such many writable color cells are needed.\n"
"  -rows n\t\tdisplay n rows at startup.\n"
"  -columns n\t\tdisplay n columns at startup.\n"
"  -tilewidth n\t\tdisplay n colored blanks in each line.\n"
"  -file file\t\tsets also the rgb.txt file to read.\n"
"  -grabcursor cursor\tsets the cursor to use for color grabbing.\n"
"\t\t\t(use with care!)\n"
"  -busycursor cursor\tsets the cursor to show while converting colors.\n"
"  -maxred n\t\tsets the minimum red\n"
"  -maxgreen n\t\t\tgreen\n"
"  -maxblue n\t\t\tblue intensity to convert to white when reading input file.\n"
"  -maxintensity n\tsame but this is a minimum for red, green and blue.\n"
"\t\t\tdefaults to 255.\n"
"  -breakpercentage n\tthe integer n is the percentage *100 a color has to\n"
"\t\t\tbe like the grabbed one to be displayed.\n"
"  -cstyle\t\toutput colors in format easy to insert in C-programs.\n"
"  -resourcestyle\toutput colors in format easy to insert in resource specs.\n"
"\t\t\t(not supported for input color format.)\n"
"  -internal\t\tdisplay colorvalues defined in input file.\n"
"  -realized\t\tdisplay colorvalues realizable on the current Xserver.\n"
"  -input, -rgb4, -rgb8, -rgb12, -rgb16, -rgbi, -ciexyz, -cieuvy, -ciexyy,\n"
"  -cielab, -cieluv, -tekhvc, -#4, -#8, -#12, or -#16\n"
"\t\t\tconvert colors to that colorspace.\n"
"\n"
"WARNING: Conversion to tekhvc needs much CPU-time!\n\n"
"Unique abbreviations of the above options are also allowed.\n"
 
, myname) ;
exit(1);
}

void exerr(char *mess)
{
int olderr=errno;

if(mess)
	fprintf(stderr,"%s:%s\n",myname,mess);
fprintf(stderr,"Error was %d",olderr);
errno=olderr;
perror(" - ");
exit(olderr);
}

void quit(Widget w, XtPointer a, XtPointer b)
{
XtDestroyApplicationContext(mycontext);
exit(0);
}

void info(Widget w, XtPointer a, XtPointer b)
{
static String infoline=NULL;

if(!infoline)
	infoline=XtMalloc(strlen(appres.info_message)+30L);
sprintf(infoline,appres.info_message,GetNumEntries(text),
		GetNumDifferent(text),GetNumRealisable(text));
XtSetArg (args[0], XtNlabel, infoline);
XtSetValues(message, args, 1);
}

void set_color(Widget w, XtPointer a, XtPointer b)
{
static XColor newback,newfore;
static Boolean backall=False,foreall=False;
register String ptr,templ;

if(w==reverse)
	{
	XColor tmpc;
	Boolean tmpb;
	Pixel a,b;

	tmpc=newback;
	newback=newfore;
	newfore=tmpc;

	tmpb=backall;
	backall=foreall;
	foreall=tmpb;
	
	XtSetArg (args[0], XtNforeground, &a);
	XtSetArg (args[1], XtNbackground, &b);
	XtGetValues(text, args, 2);

	XtSetArg (args[0], XtNforeground, b);
	XtSetArg (args[1], XtNbackground, a);
	XtSetValues(text, args, 2);

	ptr=appres.info_reverse;
	goto pstring;	
	}
if(w==defbutt)
	{
	ptr=appres.info_default;
	defload:
	if(backall==True)
		{
		backall=False;
		XFreeColors(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),
			&newback.pixel,1,0l);
		}
	if(foreall==True)
		{
		foreall=False;
		XFreeColors(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),
			&newfore.pixel,1,0l);
		}
	XtSetArg (args[0], XtNforeground, deffore);
	XtSetArg (args[1], XtNbackground, defback);
	XtSetValues(text, args, 2);
	pstring:
	XtSetArg (args[0], XtNlabel, ptr);
	XtSetValues(message, args, 1);
	}
else	if(w==setback)
	{
	ptr=GetColorName(text);
	if(!ptr){
		ptr=appres.info_nocol;
		goto pstring;
		}
	if(backall==True)
		{
		backall=False;
		XFreeColors(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),
			&newback.pixel,1,0l);
		}
	GetColorSpec(text,&newback);
	if(!XAllocColor(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),&newback))
		{
		ptr=appres.info_outof;
		goto defload;
		}
	backall=True;
        XtSetArg (args[0], XtNbackground, (Pixel)newback.pixel);
	templ=appres.info_back;
	colset:
        XtSetValues(text, args, 1);
	allocmess(strlen(templ)+strlen(ptr));
	sprintf(messbuf,templ,ptr);
	ptr=messbuf;
	goto pstring;
	}
else	{
	ptr=GetColorName(text);
	if(!ptr){
		ptr=appres.info_nocol;
		goto pstring;
		}
	if(foreall==True)
		{
		foreall=False;
		XFreeColors(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),
			&newfore.pixel,1,0l);
		}
	GetColorSpec(text,&newfore);
	if(!XAllocColor(XtDisplay(text),XDefaultColormapOfScreen(XtScreen(text)),&newfore))
		{
		ptr=appres.info_outof;
		goto defload;
		}
	foreall=True;
        XtSetArg (args[0], XtNforeground, (Pixel)newfore.pixel);
	templ=appres.info_fore;
	goto colset;
	}
}

void grabcolor(Widget w, XtPointer a, XtPointer b)
{
register String ptr;
register size_t needed;
int tmp;

if(w==text)
	{
	answer= *(RgbTextAnswer *)b;
	}
else if(w==grab)
	{
	XtSetArg (args[0], XtNlabel, appres.info_grabinit);
	XtSetValues(message, args, 1);
	GrabColor(text, (RgbTextAnswerProc)grabcolor);
	return;
	}
else	{
	answer.offset=1;
	FindColor(text, NULL, &answer);
	}
if(answer.offset)
	{
	ptr=appres.info_grab;
	needed=20;
	}
else	{
	ptr=appres.info_nohit;
	needed=0;
	}

XtSetArg(args[0],XtNsensitive,(answer.offset<answer.numhits)?True:False);
XtSetValues(next, args, 1);
XtSetArg(args[0],XtNsensitive,False);
XtSetValues(prev, args, 1);
XtSetArg(args[0],XtNsensitive,False);
XtSetValues(best, args, 1);
needed+=strlen(ptr)+strlen(answer.repres);
allocmess(needed);
if(answer.offset)
	sprintf(messbuf,ptr,answer.repres,
		(unsigned long)(answer.numhits),
		(unsigned long)(answer.numexact),
		((int)(answer.percentage))/100,
		((int)(answer.percentage))%100);
else	{
	tmp=0;
	XtSetArg(args[0],XtNbreakPercentage,&tmp);
	XtGetValues(text, args, 1);
	sprintf(messbuf,ptr,answer.repres,tmp/100,tmp%100);
	}
XtSetArg (args[0], XtNlabel, messbuf);
XtSetValues(message, args, 1);
}

void nexthit(Widget w, XtPointer a, XtPointer b)
{
register String ptr,templ;
register size_t needed;
int tmp;
Boolean flag;

if(w==next)
	answer.offset++;	
else	{
	answer.offset--;
	if(!answer.offset)
		answer.offset=1;
	}
FindColor(text, NULL, &answer);
XtSetArg(args[0],XtNsensitive,(answer.offset<answer.numhits)?True:False);
XtSetValues(next, args, 1);
XtSetArg(args[0],XtNsensitive,(answer.offset>=2)?True:False);
XtSetValues(prev, args, 1);
XtSetArg(args[0],XtNsensitive,(answer.offset>=2)?True:False);
XtSetValues(best, args, 1);
allocmess(strlen(answer.repres)+strlen(appres.info_next)+30L);
sprintf(messbuf,appres.info_next,(unsigned long)answer.offset,
		((int)(answer.percentage))/100,
		((int)(answer.percentage))%100,
		(unsigned long)(answer.numhits),
		(unsigned long)(answer.numexact));
XtSetArg (args[0], XtNlabel, messbuf);
XtSetValues(message, args, 1);
}

void allocmess(size_t needed)
{
if(messlen<needed)
	{
	if(messbuf)
		XtFree(messbuf);
	messbuf=XtMalloc(messlen=needed+30L);
	}
}

void dispsel(Widget w, XtPointer a, XtPointer b)
{
register size_t i;
String ptr;
Boolean new_state;
Cursor old_text, old_pane, old_hpane, old_htext;

if((w==comma)||(w==realized))
	{
	XtSetArg (args[0], XtNstate, &new_state);
	XtGetValues(w, args, 1);
	/* Notice this is in two, Yeah... pretty dirty, eeh? */
	XtSetArg(args[2], (w==comma)?XtNcomma:XtNrealized, new_state);

	goto set_val;
	}
else for(i=0;i<(sizeof(entry)/sizeof(entrytype));i++)
	{
	if(entry[i].sme==w)
		{
		/* Notice this is in two, Yeah... pretty dirty, eeh? */
		XtSetArg (args[2], XtNdisplayFormat, entry[i].value);

		set_val:
		XtSetArg (args[0], XtNcursor, &old_text);
		XtGetValues(text, args, 1);
		XtSetArg (args[0], XtNcursor, &old_pane);
		XtGetValues(pane, args, 1);
		if(help_shell)
			{
			XtSetArg (args[0], XtNcursor, &old_htext);
			XtGetValues(help_text, args, 1);
			XtSetArg (args[0], XtNcursor, &old_hpane);
			XtGetValues(help_pane, args, 1);
			}
		XtSetArg (args[0], XtNcursor, appres.busy_cursor);
		XtSetValues(text, args, 1);
		XtSetValues(pane, args, 1);
		if(help_shell)
			{
			XtSetValues(help_text, args, 1);
			XtSetValues(help_pane, args, 1);
			}
		XFlush(XtDisplay(message));		
		XtSetValues(text, args+2, 1);
		XtSetArg (args[0], XtNcursor, old_text);
		XtSetValues(text, args, 1);
		XtSetArg (args[0], XtNcursor, old_pane);
		XtSetValues(pane, args, 1);
		if(help_shell)
			{
			XtSetArg (args[0], XtNcursor, old_htext);
			XtSetValues(help_text, args, 1);
			XtSetArg (args[0], XtNcursor, old_hpane);
			XtSetValues(help_pane, args, 1);
			}
		return;
		}
	}
}

void show_help(Widget w, XtPointer a, XtPointer b)
{
int fd;
String fname;
Pixmap icon_pixmap = None;

XtSetArg(args[0],XtNsensitive,False);
XtSetValues(help, args, 1);
if(help_shell)
	{
	XtPopup(help_shell,XtGrabNone);
	return;
	}
XtSetArg (args[0], XtNwindowGroup, &myshell);
help_shell=XtCreatePopupShell("helpshell", topLevelShellWidgetClass,help,args,1);
XtOverrideTranslations(help_shell,XtParseTranslationTable(translations));
XtSetArg (args[0], XtNiconPixmap, &icon_pixmap);
XtGetValues(help_shell, args, 1);
if (icon_pixmap == None)
	{
	XtSetArg(args[0], XtNiconPixmap, &icon_pixmap);
	XtGetValues(myshell, args, 1);
	XtSetArg(args[0], XtNiconPixmap, icon_pixmap);
	XtSetValues(help_shell, args, 1);
	}

help_pane=XtCreateManagedWidget("rootpane",panedWidgetClass,help_shell,NULL,0);

XtSetArg (args[0], XtNshowGrip, False);
help_form=XtCreateManagedWidget("textform", formWidgetClass,help_pane,args,1);

XtSetArg (args[0], XtNresizeToPreferred, True);
XtSetArg (args[1], XtNskipAdjust, True);
help_buttonbox=XtCreateManagedWidget("buttonbox", boxWidgetClass,help_pane,args,2);

help_close=XtCreateManagedWidget("close", commandWidgetClass,help_buttonbox,NULL,0);
XtAddCallback(help_close,XtNcallback,close_help,NULL);

/* Ok, now see if we have a help file: */
/* By the way we could have used access also, but that would have used the real uid.
   in case someone calls us SUID (why?) This may lead to strange (at least unexpected
   to the user, who may set this for example to access an otherwise readprotected (Again: why?)
   help file) results. So to avoid this we open and close the file instead and accept the additional
   overhead. Of course there is a race condition herein because someone may delete or denie read
   permission to a previously accessable file before the AsciiTextWidget reopens it. However if we
   would start making this tool foolproof in that way it would have been never finished.... ;-)
   (BTW. This comment is almost completely useless) */

XtSetArg (args[0], XtNtype, XawAsciiFile); /* Assume we have a help file by default..*/
if(strlen(appres.help_file))
	fd=open(fname=appres.help_file,O_RDONLY,0); /* First search explicit set file */
else	fd= -1;	/* No forced help file */
if(fd>=0)
	close(fd); /* Was just a try... */
else	{
	/* Now look in the Database... */
	fname=XtResolvePathname(XtDisplay(myshell),"help",NULL,"",NULL,NULL,0,NULL);
	if(!fname)
		fname=XtResolvePathname(XtDisplay(myshell),"help",NULL,".help",NULL,NULL,0,NULL);
	if(!fname)
		fname=XtResolvePathname(XtDisplay(myshell),"",NULL,".help",NULL,NULL,0,NULL);
	if(!fname)
		{
		/* Search working directory too... */
		fd=open(fname="Xcolorsel.help",O_RDONLY,0);
		if(fd<0)
			{
			/* create default message.. */
			fname=XtMalloc(strlen(default_help)+strlen(appres.help_file)+10);
			sprintf(fname,default_help,strlen(appres.help_file)?appres.help_file:"<none>");

			XtSetArg (args[0], XtNtype, XawAsciiString);
			}
		else	close(fd); /* Was just a try (again)... */
		}
	}
XtSetArg (args[1], XtNstring,fname);
XtSetArg (args[2], XtNeditType,XawtextRead);
help_text=XtCreateManagedWidget("text",asciiTextWidgetClass,help_form,args,3);

XtPopup(help_shell,XtGrabNone);
XSetWMProtocols(XtDisplay(help_shell),XtWindow(help_shell),&wm_delete_window,1);
}

void close_help(Widget w, XtPointer a, XtPointer b)
{
if(help_shell)
	{
	XtPopdown(help_shell);
	}
XtSetArg(args[0],XtNsensitive,True);
XtSetValues(help, args, 1);
}

void wm_protocols(Widget w, XEvent *event , String *params, Cardinal *num_params)
{
if(event->type != ClientMessage)
	return;
if(event->xclient.data.l[0]==wm_delete_window)
	{
	if(w==help_shell)
		close_help(NULL, NULL, NULL);
	else if(w==myshell)
		quit(NULL, NULL, NULL);
	}
}
