/* A no-longer simple X program to display and edit xconq image families.
   Copyright (C) 1994, 1995, 1996 Massimo Campostrini & Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* note: display of three-color images (mono+mask) is controlled 
   by the resource "maskColor" (see XShowimf-color.ad) or by the 
   command-line argument "-mc" */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Xresource.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>
#include <X11/cursorfont.h>
#include <bitmaps/check.b>

#include "config.h"
#include "misc.h"
#include "lisp.h"
#include "imf.h"
#include "ximf.h"
#include "xutil.h"

/* MAXROWS, MAXCOLS, MINROWS, and MINCOLS can be changed safely */
#define MAXROWS 50
#define MAXCOLS 16
#define MINROWS 2
#define MINCOLS 8

#define MAXFAMS (MAXROWS*MAXCOLS)

extern int numimages;
int numfamilies;
extern ImageFamily **images;
extern char *outdirname;

/* the number of allowed magnifications NUMMAGN 
   and the magnification scales magnif can be changed safely */
#define NUMMAGN 4
int magnif[NUMMAGN] = { 1, 2, 4, 6 };

typedef enum _prep {
  prep_none,
  prep_delete,
  prep_update,
  prep_export
} Prep_type;

typedef struct a_image_stuff {
  int w, h;
  Widget mono_w, mask_w, comb_w, colr_w;
  Pixmap mono_p[NUMMAGN], mask_p[NUMMAGN], comb_p[NUMMAGN], colr_p[NUMMAGN];
  struct a_image_stuff *next;
} ImageStuff;

typedef struct a_family_stuff {
  Widget image, name;
  Widget shell, form, label, increase_magn, decrease_magn, close,
         delete, update, export;
  int imagn, changed;
  Prep_type prep;
  ImageStuff *images;
} FamilyStuff;

FamilyStuff family[MAXFAMS];

int basew=32, baseh=32;

int cols, rows, color_comb;

Arg tmpargs[10];

Widget toplevel;
Widget main_form;
Widget mainviewp;
Widget viewform;
Widget help;
Widget help_shell = NULL;
Widget select_widget;
Widget deselect;
Widget toggle;
Widget read_button;
Widget save_button;
Widget delete;
Widget quit_button;
Widget message;

char buffer[200], shortbuf[100];

XtAppContext app_con;

Display *dpy;
Window rootwin, win;
XrmDatabase xrdb;
Pixel mask_pixel;
Colormap cmap;
int depth, screen;
XVisualInfo vinfo;
Pixmap check;
char *read_suggest="", *write_suggest="";

XrmOptionDescRec xoptions[] = {
    { "-geometry",	"*geometry",	XrmoptionSepArg,	NULL },
    { "-xrm",		NULL,		XrmoptionResArg,	NULL }
};

static String fallback_resources[] = {
"XShowimf*Command.Font:		-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*",
"XShowimf*Label.Font:		-adobe-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*",
"XShowimf*Toggle.Font:		-adobe-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*",
"XShowimf*increase_magn.Label:	+ magn",
"XShowimf*decrease_magn.Label:	- magn",
"XShowimf*mainViewport.height:	484",
"XShowimf*mainForm.?.top:	ChainTop",
"XShowimf*mainForm.?.bottom:	ChainTop",
"XShowimf*mainForm.?.left:	ChainLeft",
"XShowimf*mainForm.?.right:	ChainLeft",
"XShowimf*mainForm.mainViewport.bottom:	ChainBottom",
"XShowimf*mainForm.mainViewport.right:	ChainRight",
"XShowimf*viewForm.?.top:	ChainTop",
"XShowimf*viewForm.?.bottom:	ChainTop",
"XShowimf*viewForm.?.left:	ChainLeft",
"XShowimf*viewForm.?.right:	ChainLeft",
"XShowimf*form.label.width:	310",
"XShowimf*message.width:	365",

"XShowimf*helpText.font:	-adobe-times-medium-r-*-*-14-*-*-*-*-*-*-*",
"XShowimf*help.title:		xshowimf help",
"XShowimf*helpDone.Label:	done",
"XShowimf*helpText.label:\
\\    MAIN WINDOW\\n\
help:  if you see this, you know what it does\\n\
select:  select all families\\n\
deselect:  deselect all families\\n\
toggle:  toggle selection\\n\
read:  read a imf/xbm/xpm file\\n\
save:  save selected families to an imf file\\n\
delete:  delete selected families\\n\
quit:  terminate\\n\
 \\n\
family icons:  click to popup a close-up window\\n\
family names:  click to toggle selection\\n\
 \\n\
    CLOSE-UP WINDOWS\\n\
+ magn:  increase mgnification\\n\
- magn:  decrease mgnification\\n\
delete:  delete a single image\\n\
update:  re-read an image from disk\\n\
export:  export an image to disk \\n\
close:  pop down this window\\n\
 \\n\
There are no edit buttons (yet); to edit an\\n\
image, export it to disk, edit it with your\\n\
favorite paint program (in the directory\\n\
selected by the \"-o\" option), and update.",

"selFile.selFileForm.Font: 	-adobe-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*",
"selFile.selFileForm.Label.Font:   -adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*",
"selFile.selFileForm.Command.Font: -adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*",
"selFile*Scrollbar.thickness:	6",
"selFile*selFilePrompt.height:	30",
  NULL
};

void show_image   PARAMS ((Widget w, XEvent *event, String *params, Cardinal *num_params));
void show_family  PARAMS ((Widget w, XEvent *event, String *params, Cardinal *num_params));
void image_action PARAMS ((Widget w, XEvent *event, String *params, Cardinal *num_params));

static XtActionsRec  actions[] = {
  { "ShowImage",   show_image   },       /* set info line for this image */
  { "ShowFamily",  show_family  },       /* set info line for family */
  { "ImageAction", image_action },       /* do image stuff */
};

/* translation table for label widgets for individual images */
static char Translations[] = 
  "<EnterWindow>:  ShowImage() \n\
   <LeaveWindow>:  ShowFamily() \n\
   <BtnDown>:      ImageAction() ";

void do_help();
void done_help();
void do_select();
void do_deselect();
void do_toggle();
void do_read();
void do_save();
void do_delete();
void do_quit();
void do_open_family();
void do_prep_delete();
void do_prep_update();
void do_prep_export();
void do_close_family();
void do_increase_magn();
void do_decrease_magn();

void show_image_families PARAMS ((int first_time));
void hide_image_families PARAMS ((void));

void usage PARAMS ((void));
void build_name PARAMS ((char *name, char *first, char *second));
void display_family PARAMS ((int i, int imagn));
void undisplay_family PARAMS ((int i));
ImageStuff *init_ims PARAMS ((Image *img, Widget parent));
int shell_index PARAMS ((Widget w));
Pixmap magnify_bitmap PARAMS ((char *data, int h, int w, int s));
Pixmap magnify_colrpix PARAMS ((Image *img, int s, Pixel bg_pix, Pixmap mask));
void reset_prep PARAMS ((int i));
void set_cursor PARAMS ((FamilyStuff *fms, Cursor cursor));
int delete_image PARAMS ((Widget w));
int update_or_export_image PARAMS ((Widget w, int flag));
Image *find_image PARAMS ((ImageFamily *imf, int w, int h));
void destroy_family PARAMS ((int i));
void delete_family PARAMS ((int i));
int empty_family PARAMS ((int i)); 
void mark_changed PARAMS ((ImageFamily *imf, int dummy));

#ifdef HAVE_SELFILE
extern FILE *XsraSelFile();
#endif

int
main(argc, argv)
int argc;
char *argv[];
{
    char *arg, *mask_color_name = NULL;
    char *stype;
    XrmValue val;
    XColor color, junk;
    int i, nfsave;
    
    for (i = 0; i < MAXFAMS; i++) {
	family[i].shell = NULL;
	family[i].changed = FALSE;
    }
    
    init_lisp();
    
    /* toplevel X stuff */
    toplevel =
      XtAppInitialize(&app_con, "XShowimf",
		      xoptions, XtNumber(xoptions), &argc, argv,
		      fallback_resources, NULL, 0);
    XtAppAddActions(app_con, actions, XtNumber(actions));
    dpy = XtDisplay(toplevel);
    rootwin = DefaultRootWindow(dpy);
    screen = DefaultScreen(dpy);
    
    /* process non-X argument */
    for (i = 1; i < argc; ++i) {
	arg = argv[i];
	if (arg && !strcmp(arg, "-mc")) {
	    if (i + 1 < argc) {
		mask_color_name = xmalloc((2 + strlen(argv[i + 1])) * sizeof(char));
		strcpy(mask_color_name, argv[i + 1]);
		argv[i] = NULL;
		argv[i + 1] = NULL;
	    } else {
		low_init_error("No color following -mc");
		usage();
	    }
	} else if (arg && (!strcmp(arg, "-help") || !strcmp(arg, "--help"))) {
	    usage();
	} else if (arg && !strcmp(arg, "-o")) {
	    if (i+1 < argc) {
		outdirname = argv[i+1];
		/* Blast the arg because we'll be scanning the args again
		   and we want to ignore it then. */
		argv[i] = NULL;
		argv[i+1] = NULL;
		++i;
	    } else {
		low_init_error("No output directory following -o");
		usage();
	    }
	}
    }
    
    /* read the files */
    for (i = 1; i < argc; ++i) {
	if (argv[i] != NULL) {
	    read_suggest = write_suggest = argv[i];
	    /* try to guess the format and read the file */
	    if (!read_any_file(argv[i], NULL)) {
		run_warning("Couldn't read \"%s\"", argv[i]);
	    }
	}
    }
    
    main_form =
      XtVaCreateManagedWidget("mainForm", formWidgetClass, toplevel,
			      NULL);
    
    help =
      XtVaCreateManagedWidget("help", commandWidgetClass, main_form,
			      NULL);
    XtAddCallback (help, XtNcallback, do_help, NULL);
    
    select_widget =
      XtVaCreateManagedWidget("select", commandWidgetClass, main_form,
			      XtNfromHoriz, help,
			      NULL);
    XtAddCallback (select_widget, XtNcallback, do_select, NULL);
    
    deselect =
      XtVaCreateManagedWidget("deselect", commandWidgetClass, main_form,
			      XtNfromHoriz, select_widget,
			      NULL);
    XtAddCallback (deselect, XtNcallback, do_deselect, NULL);
    
    toggle =
      XtVaCreateManagedWidget("toggle", commandWidgetClass, main_form,
			      XtNfromHoriz, deselect,
			      NULL);
    XtAddCallback (toggle, XtNcallback, do_toggle, NULL);
    
    read_button =
      XtVaCreateManagedWidget("read", commandWidgetClass, main_form,
			      XtNfromHoriz, toggle,
			      NULL);
    XtAddCallback (read_button, XtNcallback, do_read, NULL);
    
    save_button =
      XtVaCreateManagedWidget("save", commandWidgetClass, main_form,
			      XtNfromHoriz, read_button,
			      NULL);
    XtAddCallback (save_button, XtNcallback, do_save, NULL);
    
    delete =
      XtVaCreateManagedWidget("delete", commandWidgetClass, main_form,
			      XtNfromHoriz, save_button,
			      NULL);
    XtAddCallback (delete, XtNcallback, do_delete, NULL);
    
    quit_button =
      XtVaCreateManagedWidget("quit", commandWidgetClass, main_form,
			      XtNfromHoriz, delete,
			      NULL);
    XtAddCallback (quit_button, XtNcallback, do_quit, NULL);
    
    message =
      XtVaCreateManagedWidget("message", labelWidgetClass, main_form,
			      XtNfromVert, help,
			      NULL);
    
    nfsave = numfamilies = numimages;
    if (numfamilies>MAXFAMS) {
	numfamilies = MAXFAMS;
    }
    
    mainviewp = XtVaCreateManagedWidget("mainViewport", viewportWidgetClass,
					main_form,
					XtNfromVert,   message,
					XtNallowVert,  True,
					NULL);
    cols = MINCOLS;
    show_image_families(TRUE);
    
    XtRealizeWidget(toplevel);
    
    /* several of the following functions work only
       after XtRealizeWidget has been called */
    
    sprintf(buffer, "%d image families found", numfamilies);
    if (nfsave>MAXFAMS) {
	sprintf(buffer, "%d image families found, but I can display only %d",
		nfsave, MAXFAMS);
    }
    XtVaSetValues(message, XtNlabel, buffer, NULL);
    
    xrdb = XtDatabase(dpy);
    win = XtWindow(toplevel);
    cmap = XDefaultColormap(dpy,screen);
    depth = DefaultDepth(dpy,screen);
    
    if (XrmGetResource(xrdb, "xshowimf.maskColor",
		       "XShowimf.maskColor", &stype, &val)) {
	if (strcmp(stype, "String")) {
	    fprintf(stderr, "resource type for maskColor is %s, ignoring\n", stype);
	} else {
	    /* command line argument takes precedence over resource */
	    if (!mask_color_name) {
		mask_color_name = val.addr;
	    }
	}
    }
    
    check = XCreateBitmapFromData(dpy, rootwin, 
				  (char *) check_bits, check_width, check_height);
    
    /* only depths 4, 8, 16, 24, and 32 are supported */
    if ((depth == 4 || depth % 8 == 0) && mask_color_name) {
	color_comb = 1;
	if (XAllocNamedColor(dpy, cmap, mask_color_name, &color, &junk)) {
	    mask_pixel = color.pixel;
	} else {
	    mask_pixel = XBlackPixel(dpy, screen);
	}
    } else {
	color_comb = 0;
    }
    
    XtAppMainLoop (app_con);
    
    return 0;
}

void
show_image_families(first_time)
int first_time;
{
    int i, a, needw, needh, vd, hd;
    static int vert_distance, horiz_distance, maxrows;
    Image *img;
    X11Image *ximg;
    XWindowChanges wchanges;
    Dimension fw, fh, vh, dh, sw;
    Widget dummy, vert;
    static int oldrows = 0, oldcols = 0;
    
    /* pick a number of columns that gives a nice display */
    for (; cols<=MAXCOLS; cols++) {
	if (2*cols*cols>3*numfamilies && cols*MAXROWS>=numfamilies)  break;
    }
    rows = (numfamilies+cols-1)/cols;
    rows = max(rows,MINROWS);
    
    if (first_time) {
	/* 1st pass: create dummy widgets just to get values
	   and to estimated needed height and width */

	/* be sure we get a vertical scrollbar */
	XtVaGetValues(mainviewp, XtNheight, &vh, NULL);
	viewform =
	  XtVaCreateManagedWidget("viewForm", formWidgetClass, mainviewp,
				  XtNheight, 2 * vh,
				  NULL);
	dummy =
	  XtVaCreateManagedWidget("dummy", commandWidgetClass, viewform,
				  NULL);
	vert = XtNameToWidget(mainviewp, "vertical");
	if (vert) {
	    XtVaGetValues(vert,  XtNwidth, &sw,  NULL);
	} else {
	    sw = 10;
	}
	XtVaGetValues(dummy,
		      XtNvertDistance, &vd,
		      XtNhorizDistance, &hd,
		      XtNheight, &dh,
		      NULL);
	XtDestroyWidget(dummy);
	vert_distance = baseh+4 + dh+1 + vd;
	horiz_distance = basew+8 + 2+hd;
	maxrows = vh/vert_distance;
	needh = vert_distance*rows + vd;
	needw = horiz_distance*cols + hd;
	if (vh < 20)
	  vh = needh;
    
	XtDestroyWidget(viewform);
	XtVaSetValues(mainviewp,
		      XtNheight, min(needh, vh),
		      XtNwidth, needw+sw+1,
		      NULL);

	/* 2nd pass: create the real widgets */

	viewform =
	  XtVaCreateManagedWidget("viewForm", formWidgetClass, mainviewp,
				  XtNheight, needh,
				  XtNwidth, needw+sw+1,
				  NULL);
    } else {
	/* not first_time */
	if ((rows != oldrows && (oldrows < maxrows || rows < maxrows))
	    || cols != oldcols) {
	    /* ask the window manager to change main window size */
	    XtVaGetValues(main_form,
			  XtNheight, &fh,
			  XtNwidth,  &fw,
			  NULL);
	    wchanges.width  = fw + (cols-oldcols)*horiz_distance;
	    wchanges.height =
	      fh + (min(rows,maxrows)-min(oldrows,maxrows))*vert_distance;
	    XReconfigureWMWindow(dpy, XtWindow(toplevel), screen, 
				 CWWidth | CWHeight, &wchanges);
	}
    }
    
    oldrows = rows;
    oldcols = cols;
    
    /* entries for each image family */
    for (i = 0; i < numfamilies; i++) {

	/* picture */
	a = 0;
	XtSetArg(tmpargs[a], XtNwidth,  basew+8);  a++;
	XtSetArg(tmpargs[a], XtNheight, baseh+4);  a++;
	XtSetArg(tmpargs[a], XtNlabel, "");  a++;
	x11_interp_imf(dpy, rootwin, images[i], TRUE);
	img = best_image(images[i], basew, baseh);
	if (img->hook != NULL) {
	    ximg = (X11Image *) img->hook;
	    if (ximg->colr != None) {
		XtSetArg(tmpargs[a], XtNbitmap, ximg->colr);  a++;
	    } else if (ximg->mono != None) {
		XtSetArg(tmpargs[a], XtNbitmap, ximg->mono);  a++;
	    } else if (ximg->mask != None) {
		XtSetArg(tmpargs[a], XtNbitmap, ximg->mask);  a++;
	    }
	}
	if (i >= cols) {
	    XtSetArg(tmpargs[a], XtNfromVert,  family[i-cols].name);  a++;
	}
	if (i % cols) {
	    XtSetArg(tmpargs[a], XtNfromHoriz, family[i-1].image);  a++;
	}

	build_name(buffer, "image_", images[i]->name);
	family[i].image =
	  XtCreateManagedWidget(buffer, commandWidgetClass, viewform,
				tmpargs, a);
	XtAddCallback (family[i].image, XtNcallback, do_open_family, NULL);

	/* name */
	build_name(buffer, "name_", images[i]->name);
	a = 0;
	XtSetArg(tmpargs[a], XtNwidth, basew+8);  a++;
	XtSetArg(tmpargs[a], XtNlabel, images[i]->name);  a++;
	XtSetArg(tmpargs[a], XtNfromVert, family[i].image);  a++;
	XtSetArg(tmpargs[a], XtNvertDistance, -1);  a++;
	if (i % cols) {
	    XtSetArg(tmpargs[a], XtNfromHoriz, family[i-1].name);  a++;
	}
	family[i].name =
	  XtCreateManagedWidget(buffer, toggleWidgetClass, viewform,
				tmpargs, a);
    }
}

void
hide_image_families()
{
    int i;

    /* entries for each image family */
    for (i = 0; i < numfamilies; i++) {
	if (family[i].image)
	  XtDestroyWidget(family[i].image);
	if (family[i].name)
	  XtDestroyWidget(family[i].name);
  }
}

void
usage()
{
    fprintf(stderr,
	  "usage: xshowimf [-mc mask color] [-o outdir] imfile ...\n");
    exit(1);
}

/* popup a window displaying all the images in the family */

void
display_family(i, imagn)
int i;
int imagn;
{
    int img0;
    Widget last, up;
    Image *img;
    FamilyStuff *fms = &family[i];
    ImageStuff *ims;

    fms->imagn = imagn;

    build_name(buffer, "xshowimf_", images[i]->name);
    fms->shell = XtVaCreatePopupShell(buffer, topLevelShellWidgetClass,
				      toplevel, NULL);

    fms->form =
      XtVaCreateManagedWidget("form", formWidgetClass, fms->shell,
			      NULL);

    fms->increase_magn =
      XtVaCreateManagedWidget("increase_magn", commandWidgetClass, fms->form, 
			      NULL);
    XtAddCallback (fms->increase_magn, XtNcallback, do_increase_magn, NULL);

    fms->decrease_magn =
      XtVaCreateManagedWidget("decrease_magn", commandWidgetClass, fms->form, 
			      XtNfromHoriz, fms->increase_magn,
			      NULL);
    XtAddCallback (fms->decrease_magn, XtNcallback, do_decrease_magn, NULL);

    fms->delete =
      XtVaCreateManagedWidget("delete", commandWidgetClass, fms->form, 
			      XtNfromHoriz, fms->decrease_magn,
			      NULL);
    XtAddCallback (fms->delete, XtNcallback, do_prep_delete, NULL);

    fms->update =
      XtVaCreateManagedWidget("update", commandWidgetClass, fms->form, 
			      XtNfromHoriz, fms->delete,
			      NULL);
    XtAddCallback (fms->update, XtNcallback, do_prep_update, NULL);

    fms->export =
      XtVaCreateManagedWidget("export", commandWidgetClass, fms->form, 
			      XtNfromHoriz, fms->update,
			      NULL);
    XtAddCallback (fms->export, XtNcallback, do_prep_export, NULL);

    fms->close =
      XtVaCreateManagedWidget("close", commandWidgetClass, fms->form, 
			      XtNfromHoriz, fms->export,
			      NULL);
    XtAddCallback (fms->close, XtNcallback, do_close_family, NULL);

    fms->label =
      XtVaCreateManagedWidget("label", labelWidgetClass, fms->form,
			      XtNfromVert, fms->increase_magn,
			      NULL);

    last = fms->label;
    img0 = 1;
    for (img = images[i]->images; img != NULL; img = img->next) {
	if (last)
	  up = last;
	last = NULL;

	if (img->hook) {
	    if (img0) {
		if (!fms->images || fms->changed) {
		    fms->images = init_ims(img, fms->form);
		}
		ims = fms->images;
		img0 = 0;
	    } else {
		if (!ims->next || fms->changed) {
		    ims->next = init_ims(img, fms->form);
		}
		ims = ims->next;
	    }
	    
	    if (ims->mono_p[imagn] != None) {
		sprintf(buffer, "mono-%dx%d", img->w, img->h);
		ims->mono_w = last =
		  XtVaCreateManagedWidget(buffer, labelWidgetClass, fms->form,
					  XtNbitmap, ims->mono_p[imagn],
					  XtNfromHoriz, last,
					  XtNfromVert, up,
					  NULL);
		XtOverrideTranslations(ims->mono_w,
				       XtParseTranslationTable(Translations));
	    } else {
		ims->mono_w = NULL;
	    }
	    if (ims->mask_p[imagn] != None) {
		sprintf(buffer, "mask-%dx%d", img->w, img->h);
		ims->mask_w = last =
		  XtVaCreateManagedWidget(buffer, labelWidgetClass, fms->form,
					  XtNbitmap, ims->mask_p[imagn],
					  XtNfromHoriz, last,
					  XtNfromVert, up,
					  NULL);
		XtOverrideTranslations(ims->mask_w,
				       XtParseTranslationTable(Translations));
	    } else {
		ims->mask_w = NULL;
	    }
	    if (ims->comb_p[imagn] != None) {
		sprintf(buffer, "comb-%dx%d", img->w, img->h);
		ims->comb_w = last =
		  XtVaCreateManagedWidget(buffer, labelWidgetClass, fms->form,
					  XtNbitmap, ims->comb_p[imagn], 
					  XtNfromHoriz, last,
					  XtNfromVert, up,
					  NULL);
		XtOverrideTranslations(ims->comb_w,
				       XtParseTranslationTable(Translations));
	    } else {
		ims->comb_w = NULL;
	    }
	    if (ims->colr_p[imagn] != None) {
		sprintf(buffer, "colr-%dx%d", img->w, img->h);
		ims->colr_w = last =
		  XtVaCreateManagedWidget(buffer, labelWidgetClass, fms->form,
					  XtNbitmap, ims->colr_p[imagn],
					  XtNfromHoriz, last,
					  XtNfromVert, up,
					  NULL);
		XtOverrideTranslations(ims->colr_w,
				       XtParseTranslationTable(Translations));
	    } else {
		ims->colr_w = NULL;
	    }
	}
    }
    fms->changed = FALSE;

    /* now show them */
    reset_prep(i);
    XtPopup(fms->shell, XtGrabNone);
    show_family(fms->label, NULL, NULL, NULL);
}

void
undisplay_family(i)
int i;
{
    FamilyStuff *fms = &family[i];
    ImageStuff *ims;

    if (!fms->shell) {
	return;
    }

    XtPopdown(fms->shell);

    for (ims = fms->images; ims; ims = ims->next) {
	if (ims->mono_w) {
	    XtDestroyWidget(ims->mono_w);
	}
	if (ims->mask_w) {
	    XtDestroyWidget(ims->mask_w);
	}
	if (ims->comb_w) {
	    XtDestroyWidget(ims->comb_w);
	}
	if (ims->colr_w) {
	    XtDestroyWidget(ims->colr_w);
	}
    }

    XtDestroyWidget(fms->label);
    XtDestroyWidget(fms->increase_magn);
    XtDestroyWidget(fms->decrease_magn);
    XtDestroyWidget(fms->close);
    XtDestroyWidget(fms->delete);
    XtDestroyWidget(fms->update);
    XtDestroyWidget(fms->export);
    XtDestroyWidget(fms->label);
    XtDestroyWidget(fms->form);
    XtDestroyWidget(fms->shell);

    fms->shell = NULL;
}

ImageStuff *
init_ims (img, parent)
Image *img;
Widget parent;
{
    int bytesize, j, w, h;
    Pixmap pix;
    X11Image *ximg;
    GC gc;
    ImageStuff *ims;
    char *rawdata;
    Pixel bg_pixel, fg_pixel;
    Widget dummy;
    
    ximg = (X11Image *) img->hook;
    if (!ximg)  return NULL;
    
    ims = (ImageStuff *) xmalloc(sizeof(ImageStuff));
    ims->next = NULL;
    for (j=0; j<NUMMAGN; j++) {
	ims->mono_p[j] = None;
	ims->mask_p[j] = None;
	ims->comb_p[j] = None;
	ims->colr_p[j] = None;
    }
    ims->h = img->h;
    ims->w = img->w;
    
    /* build pixmaps */
    if (ximg->mono != None) {
	ims->mono_p[0] = ximg->mono;
	if (img->rawmonodata) {
	    for (j=1; j<NUMMAGN; j++) {
		ims->mono_p[j] =
		  magnify_bitmap(img->rawmonodata, img->h, img->w, magnif[j]);
	    }
	}
    }
    
    if (ximg->mask != None) {
	ims->mask_p[0] = ximg->mask;
	if (img->rawmaskdata) {
	    for (j=1; j<NUMMAGN; j++) {
		ims->mask_p[j] =
		  magnify_bitmap(img->rawmaskdata, img->h, img->w, magnif[j]);
	    }
	}
    }
    
    if (ximg->mono != None && img->rawmonodata &&
	ximg->mask != None && img->rawmaskdata) {
	bytesize = ((img->w + 7) / 8) * img->h;
	
	if (!color_comb) {
	    /* monochrome */
	    rawdata = (char *) malloc(bytesize*sizeof(char));
	    if (rawdata) {
		for (j=0; j<bytesize; j++) {
		    rawdata[j] = img->rawmonodata[j] | ~img->rawmaskdata[j];
		}
		pix = XCreateBitmapFromData(dpy, rootwin, rawdata, img->w, img->h);
		ims->comb_p[0] = pix;
		for (j=1; j<NUMMAGN; j++) {
		    ims->comb_p[j] = magnify_bitmap(rawdata, img->h, img->w, magnif[j]);
		}
		free(rawdata);
	    }

	} else {
	    /* color or grayscale */
	    /* get pixels; the widget has not been created yet, so we need a dummy */
	    sprintf(buffer, "comb-%dx%d", img->w, img->h);
	    dummy = XtVaCreateManagedWidget(buffer, labelWidgetClass, parent, NULL); 
	    XtVaGetValues(dummy,
			  XtNbackground, &bg_pixel,
			  XtNforeground, &fg_pixel,
			  NULL);
	    XtDestroyWidget(dummy);
	    
	    for (j=0; j<NUMMAGN; j++) {
		w = img->w*magnif[j];
		h = img->h*magnif[j];
		pix = XCreatePixmap(dpy, rootwin, w, h, depth);
		if (!pix || pix==None)  continue;
		gc = XCreateGC(dpy, pix, 0, NULL);

		/* background */
		XSetClipOrigin(dpy, gc, 0, 0);
		XSetForeground(dpy, gc, bg_pixel);
		XFillRectangle(dpy, pix, gc, 0, 0, w, h);

		/* mask */
		XSetForeground(dpy, gc, mask_pixel);
		XSetClipMask(dpy, gc, ims->mask_p[j]);
		XFillRectangle(dpy, pix, gc, 0, 0, w, h);

		/* mono */
		XSetForeground(dpy, gc, fg_pixel);
		XSetClipMask(dpy, gc, ims->mono_p[j]);
		XFillRectangle(dpy, pix, gc, 0, 0, w, h);

		ims->comb_p[j] = pix;
	    }
	}
    }
    if (ximg->colr != None) {
	/* get bg pixel; the widget has not been created yet, so we need a dummy */
	sprintf(buffer, "colr-%dx%d", img->w, img->h);
	dummy =
	  XtVaCreateManagedWidget(buffer, labelWidgetClass, parent, NULL); 
	XtVaGetValues(dummy,
		      XtNbackground, &bg_pixel,
		      NULL);
	XtDestroyWidget(dummy);

	if (img->rawcolrdata) {
	    for (j = 0; j < NUMMAGN; j++) {
		ims->colr_p[j] =
		  magnify_colrpix(img, magnif[j], bg_pixel, ims->mask_p[j]);
	    }
	}
    }
    
    return ims;
}

/* build a X-friendly widget name */

void
build_name(name, first, second)
char *name, *first, *second;
{
    char *ch;

    strcpy(name, first);
    strcat(name, second);
    for (ch = name; *ch; ch++) {
	if (!isalnum(*ch))
	  *ch = '_';
    }
}

void
do_help(w)
Widget w;
{
    Widget w_form, w_done, w_text;
    
    if (!help_shell) {
	/* first time: create help popup */
	help_shell =
	  XtVaCreatePopupShell("help",
			       topLevelShellWidgetClass,
			       toplevel, NULL);
	w_form =
	  XtVaCreateManagedWidget("helpForm", formWidgetClass,
				  help_shell, NULL);
	w_text =
	  XtVaCreateManagedWidget("helpText", labelWidgetClass, w_form,
				  NULL);
	w_done =
	  XtVaCreateManagedWidget("helpDone", commandWidgetClass, w_form,
				  XtNfromVert, w_text,  NULL);
	XtAddCallback(w_done, XtNcallback, done_help, NULL);
    }
    XtPopup(help_shell, XtGrabNone);
    XMapRaised(XtDisplay(help_shell), XtWindow(help_shell));
}

void
done_help(w)
Widget w;
{
    XtPopdown(help_shell);
}

void
do_select(w)
Widget w;
{
    int i;

    for (i = 0; i < numfamilies; i++) {
	XtVaSetValues(family[i].name, XtNstate, (Boolean) 1, NULL);
    }
}

void
do_deselect(w)
Widget w;
{
    int i;

    for (i = 0; i < numfamilies; i++) {
	XtVaSetValues(family[i].name, XtNstate, (Boolean) 0, NULL);
    }
}

void
do_toggle(w)
Widget w;
{
    int i;
    Boolean state;

    for (i = 0; i < numfamilies; i++) {
	XtVaGetValues(family[i].name, XtNstate, &state, NULL);
	XtVaSetValues(family[i].name, XtNstate, (Boolean) !state, NULL);
    }
}

void
do_read(w)
Widget w;
{
    FILE *stream = NULL;
    char *filename;
    int i, changed;
    
#ifdef HAVE_SELFILE
    stream = XsraSelFile(toplevel,         "Read from file:              ",
			 "Okay", "Cancel", "Error: can't open file   ",
			 read_suggest, "r", NULL, &filename);
#endif
    if (!stream)
      return;
    
    read_suggest = filename;
    fclose(stream);
    if (read_any_file(filename, mark_changed)) {
	changed = numimages > numfamilies;
	for (i = 0; i < numimages; i++) {
	    changed = changed || family[i].changed;
	}
	if (changed) {
	    hide_image_families();
	    numfamilies = min(numimages,MAXFAMS);
	    for (i = 0; i < numimages; i++) {
		undisplay_family(i);
	    }
	    show_image_families(FALSE);
	}
	sprintf(buffer, "successfully read \"%s\"", filename);
    } else {
	sprintf(buffer, "failed reading \"%s\"", filename);
    }
    XtVaSetValues(message, XtNlabel, buffer, NULL);
}

void
mark_changed(imf, dummy)
ImageFamily *imf;
int dummy;
{
  int i;

  for (i = 0; i < numimages; i++) {
    if (imf==images[i]) {
      family[i].changed = TRUE;
      return;
    }
  }
}

void
do_save(w)
Widget w;
{
    int i, n;
    Boolean state;
    FILE *stream=NULL;
    char *filename;
    
    n = 0;
    for (i = 0; i < numfamilies; i++) {
	XtVaGetValues(family[i].name, XtNstate, &state, NULL);
	if (state) {
	    if (!stream) {
#ifdef HAVE_SELFILE
		stream = XsraSelFile(toplevel,
				     "Save in file:              ",
				     "Okay", "Cancel",
				     "Error: can't open file   ",
				     write_suggest, "w", NULL, &filename);
#endif
		if (stream) {
		    write_suggest = filename;
		} else {
		    return;
		}
	    }
	    /* write_imf expects rawdata in "natural" order, not in the 
	       X11 "byte-reversed" order the rest of this program uses */
	    reverse_rawdata(images[i]);
	    write_imf(stream, images[i]);
	    reverse_rawdata(images[i]);
	    n++;
	}
    }
    if (stream) {
	fclose(stream);
	if (n) {
	    sprintf(buffer, "%d image families saved in \"%s\"",
		    numfamilies, filename);
	    XtVaSetValues(message, XtNlabel, buffer, NULL);
	}
    }
}

void
do_delete(w)
Widget w;
{
    delete_family(-1);
}

void
do_quit(w)
Widget w;
{
    exit(0);
}

/* callback: popup the family corresponding to the widget 
	     do_open_family was called from */
void
do_open_family(w)
Widget w;
{
    int i;

    for (i = 0; i < numfamilies; i++) {
	if (w==family[i].image || w==family[i].name) {
	    if (family[i].shell) {
		if (family[i].changed) {
		    display_family(i, 0);
		}
		XMapRaised(XtDisplay(family[i].shell),
			   XtWindow(family[i].shell));
	    } else {
		display_family(i, 0);
	    }
	    return;
	}
    }
}

void
do_prep_delete(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];
    
    fms->prep = prep_delete;
    
    set_cursor(fms, XCreateFontCursor(dpy, XC_pirate));
    XtVaSetValues(fms->delete, XtNleftBitmap, check, NULL);
    XtVaSetValues(fms->update, XtNleftBitmap, None, NULL);
    XtVaSetValues(fms->export, XtNleftBitmap, None, NULL);
}

void
do_prep_update(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];

    fms->prep = prep_update;
    set_cursor(fms, XCreateFontCursor(dpy, XC_dot));
    XtVaSetValues(fms->update, XtNleftBitmap, check, NULL);
    XtVaSetValues(fms->delete, XtNleftBitmap, None, NULL);
    XtVaSetValues(fms->export, XtNleftBitmap, None, NULL);
}

void
do_prep_export(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];

    fms->prep = prep_export;
    set_cursor(fms, XCreateFontCursor(dpy, XC_pencil));
    XtVaSetValues(fms->export, XtNleftBitmap, check, NULL);
    XtVaSetValues(fms->delete, XtNleftBitmap, None, NULL);
    XtVaSetValues(fms->update, XtNleftBitmap, None, NULL);
}

void
reset_prep (i)
int i;
{
    FamilyStuff *fms = &family[i];

    fms->prep = prep_none;
    set_cursor(fms, XCreateFontCursor(dpy, XC_left_ptr));
    XtVaSetValues(fms->delete, XtNleftBitmap, None, NULL);
    XtVaSetValues(fms->update, XtNleftBitmap, None, NULL);
    XtVaSetValues(fms->export, XtNleftBitmap, None, NULL);
}

void
set_cursor (fms, cursor)
FamilyStuff *fms;
Cursor cursor;
{
    ImageStuff *ims;

    for (ims = fms->images; ims; ims = ims->next) {
	if (ims->mono_w) {
	    XtVaSetValues(ims->mono_w, XtNcursor, cursor, NULL);
	}
	if (ims->mask_w) {
	    XtVaSetValues(ims->mask_w, XtNcursor, cursor, NULL);
	}
	if (ims->comb_w) {
	    XtVaSetValues(ims->comb_w, XtNcursor, cursor, NULL);
	}
	if (ims->colr_w) {
	    XtVaSetValues(ims->colr_w, XtNcursor, cursor, NULL);
	}
    }
}

void
do_close_family(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));

    if (i >= numfamilies)
      return;
    undisplay_family(i);
}

void 
do_increase_magn(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));
    int imag;

    if (i >= numfamilies)
      return;

    /* already at max? */
    if ((imag = family[i].imagn+1) >= NUMMAGN)
      return;
    undisplay_family(i);
    display_family(i, imag);
}

void 
do_decrease_magn(w)
Widget w;
{
    int i = shell_index(XtParent(XtParent(w)));
    int imag;
    
    /* already at min? */
    if ((imag = family[i].imagn-1) < 0)
      return;
    undisplay_family(i);
    display_family(i, imag);
}

int 
shell_index(w)
Widget w;
{
    int i;

    for (i = 0; i < numfamilies; i++) {
	if (family[i].shell == w)
	  break;
    }

    return i;
}

Pixmap
magnify_bitmap (data, h, w, s)
char *data; 
int h, w, s;
{
    Pixmap pix;
    int lo, ln, i, j, is, js;
    char *new, *n, *o, *so, mo, mn;
    
    lo = (w + 7) / 8;
    ln = (w*s + 7) / 8;
    new = (char *) malloc(s * h* ln * sizeof(char));
    if (!new)
      return None;
    
    for (i = 0; i < s * h * ln; i++)
      new[i] = '\0';
    
    n = new-1;
    o = data-1;
    
    for (i = 0; i < h; i++) {
	so = o;
	for (is = 0; is < s; is++) {
	    o = so;
	    for (j = 0; j < w; j++) {
		if (!(j & 7)) {
		    o++;
		    mo = '\001';
		} else {
		    mo <<= 1;
		}
		for (js = 0; js < s; js++) {
		    if (!((s*j+js) & 7)) {
			n++;
			mn = '\001';
		    } else {
			mn <<= 1;
		    }
		    if (*o & mo)
		      *n |= mn;
		}
	    }
	}
    }
    
    pix = XCreateBitmapFromData(dpy, rootwin, new, s*w, s*h);
    free(new); 
    return pix;      
}


Pixmap 
magnify_colrpix (img, s, bg_pix, mask)
Image *img;
int s;
Pixel bg_pix;
Pixmap mask;
{
    int i, j, r, ri, rc, c, l,
    rsize, rowbytesize, bytesize, rmask;
    char *dp, *rp, *rpsave, *data;
    Pixmap pixmap;
    Pixel index[256], pixel;
    XImage *ximage;
    GC gc;
    X11Image *ximg = (X11Image *) img->hook;
    
    if (!ximg || !img->rawcolrdata)
      return None;
    
    /* find reverse index->pixel mapping */
    for (c=0; c<256; c++) {
	index[c] = XBlackPixel(dpy,screen);
    }
    for (c=0; c<img->numcolors; c++) {
	index[img->rawpalette[4*c]] = ximg->colpix[c];
    }
    
    /* make color data */
    rsize = img->pixelsize;
    rmask = (1<<img->pixelsize) - 1;
    rowbytesize = img->w*s * depth/8;
    bytesize = rowbytesize * img->h*s;
    data = xmalloc(bytesize*sizeof(char));
    dp = data;
    rp = img->rawcolrdata;
    for (r=0; r<img->h; r++) {
	rpsave = rp;
	for (j=0; j<s; j++) {
	    rp = rpsave;
	    ri = 8 - img->pixelsize;
	    for (c=0; c<img->w; c++) {
		rc = ((int) (*rp>>ri)) & rmask;
		if (ri) {
		    ri -= img->pixelsize;
		} else {
		    ri = 8 - img->pixelsize;
		    rp++;
		}
		pixel = index[rc];
		for (i = 0; i<s; i++) {
		    for (l=depth-8; l>=0; l-=8) {
			*dp = (pixel>>l) & 0xff;
			dp++;
		    }
		}
	    }
	    if ((img->pixelsize*img->w)%8) {
		rp++;
	    }
	}
    }
    
    /* convert to XImage */
    ximage = XCreateImage(dpy, DefaultVisual(dpy,screen), depth,
			  ZPixmap, 0, data, img->w*s, img->h*s,
			  8, rowbytesize);
    if (!ximage) {
	free(data);
	return None;
    }
    ximage->byte_order = MSBFirst;
    ximage->bitmap_bit_order = MSBFirst;
    
    /* and finally to Pixmap */
    pixmap = XCreatePixmap(dpy, rootwin, ximage->width, ximage->height, depth);
    if (!pixmap)  pixmap = None;
    if (pixmap == None) {
	XDestroyImage(ximage);
	/* XDestroyImage also frees data */
	return None;
    }
    gc = XCreateGC(dpy, pixmap, 0, NULL);
    
    /* background */
    XSetClipOrigin(dpy, gc, 0, 0);
    XSetForeground(dpy, gc, bg_pix);
    XFillRectangle(dpy, pixmap, gc, 0, 0, ximage->width, ximage->height);
    
    XSetClipMask(dpy, gc, mask);
    XPutImage(dpy, pixmap, gc, ximage, 0, 0, 0, 0,
	      ximage->width, ximage->height);
    XFreeGC(dpy, gc);
    XDestroyImage(ximage);
    /* XDestroyImage also frees data */
    
    return pixmap;
}

/* actions */

void 
show_image (w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];
    ImageStuff *ims;
    char *add;
    
    sprintf(buffer, "%s: ", images[i]->name);
    if (fms->imagn) {
	sprintf(buffer+strlen(buffer)-2, " (x %d): ", magnif[fms->imagn]);
    }
    add = buffer + strlen(buffer);
    if (i>=numfamilies)  return;
    for (ims = fms->images; ims; ims = ims->next) {
	if (w == ims->mono_w) {
	    sprintf(add, "%dx%d mono",  ims->w, ims->h);
	} else if (w == ims->mask_w) {
	    sprintf(add, "%dx%d mask",  ims->w, ims->h);
	} else if (w == ims->comb_w) {
	    sprintf(add, "%dx%d comb",  ims->w, ims->h);
	} else if (w == ims->colr_w) {
	    sprintf(add, "%dx%d color", ims->w, ims->h);
	}
    }
    if (*add) {
	XtVaSetValues(fms->label, XtNlabel, buffer, NULL);
    }
}

void 
show_family (w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];

    if (fms->imagn) {
	sprintf(buffer, "%s (x %d)                   ",
		images[i]->name, magnif[fms->imagn]);
    } else {
	sprintf(buffer, "%s                   ",
		images[i]->name);
    }
    XtVaSetValues(fms->label, XtNlabel, buffer, NULL);
}

void 
image_action (w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int i = shell_index(XtParent(XtParent(w)));
    FamilyStuff *fms = &family[i];
    
    switch (event->type) {
      case ButtonPress:
	if (event->xbutton.button == 1) {
	    if (fms->prep == prep_delete) {
		if (delete_image(w)) {
		    if (empty_family(i)) {
			delete_family(i);
			return;
		    } else {
			display_family(i, fms->imagn);
		    }
		}
	    } else if (fms->prep == prep_update) {
		update_or_export_image(w, 0);
	    } else if (fms->prep == prep_export) {
		update_or_export_image(w, 1);
	    }
	    reset_prep(i);
	}
    }   
}

ImageStuff *
find_ims(fms, w)
FamilyStuff *fms;
Widget w;
{
    ImageStuff *ims;

    for (ims = fms->images; ims; ims = ims->next) {
	if (w == ims->mono_w) {
	    return ims;
	} else if (w == ims->mask_w) {
	    return ims;
	} else if (w == ims->comb_w) {
	    return ims;
	} else if (w == ims->colr_w) {
	    return ims;
	}
    }
    return NULL;
}

int
delete_image(w)
Widget w;
{
    int j, i;
    FamilyStuff *fms;
    ImageStuff *ims;
    Image *img;
    X11Image *ximg = NULL;
    
    if (!w)  return 0;
    i = shell_index(XtParent(XtParent(w)));
    fms = &family[i];
    ims = find_ims(fms,w);
    if (!ims)  return 0;
    img = find_image(images[i], ims->w, ims->h);
    if (!img)  return 0;
    ximg = (X11Image *) img->hook;
    if (!ximg)  return 0;
    
    if (w == ims->mono_w) {
	undisplay_family(i);
	/* do not free unmagnified pixmap; they might be used in the main window */
	ims->mono_p[0] = None;
	for (j=1; j<NUMMAGN; j++) {
	    if (ims->mono_p[j] != None) {
		XFreePixmap(dpy, ims->mono_p[j]);
		ims->mono_p[j] = None;
	    }
	}
	ximg->mono = None;
	ximg->monodata = NULL;
	img->rawmonodata = NULL;
	img->monodata = lispnil;
    } else if (w == ims->mask_w) {
	undisplay_family(i);
	ims->mask_p[0] = None;
	for (j=1; j<NUMMAGN; j++) {
	    if (ims->mask_p[j] != None) {
		XFreePixmap(dpy, ims->mask_p[j]);
		ims->mask_p[j] = None;
	    }
	}
	ximg->mask = None;
	ximg->maskdata = NULL;
	img->rawmaskdata = NULL;
	img->maskdata = lispnil;
    } else if (w == ims->colr_w) {
	undisplay_family(i);
	ims->colr_p[0] = None;
	for (j=1; j<NUMMAGN; j++) {
	    if (ims->colr_p[j] != None) {
		XFreePixmap(dpy, ims->colr_p[j]);
		ims->colr_p[j] = None;
	    }
	}
	ximg->colr = None;
	ximg->colrdata = NULL;
	img->rawcolrdata = NULL;
	img->colrdata = lispnil;
    } else if (w == ims->comb_w) {
	XBell(dpy,35);
	return 0;
    }
    if (w == ims->mono_w || w == ims->mask_w) {
	for (j=0; j<NUMMAGN; j++) {
	    if (ims->comb_p[j] != None) {
		XFreePixmap(dpy, ims->comb_p[j]);
		ims->comb_p[j] = None;
	    }
	}
    }
    
    return 1;
}

int
update_or_export_image (w, flag)
Widget w;
int flag;
{
    int i, ok = 0, imag;
    FamilyStuff *fms;
    ImageStuff *ims;
    Image *img;
    FILE *fp = NULL;
    ImageFamily *imf;

    if (!w)
      return 0;
    i = shell_index(XtParent(XtParent(w)));
    fms = &family[i];
    imf = images[i];
    ims = find_ims(fms, w);
    if (!ims)
      return 0;
    img = find_image(images[i], ims->w, ims->h);
    if (!img)
      return 0;

    if (outdirname && outdirname[0]) {
	sprintf(shortbuf, "%s/%s.%dx%d.",
		outdirname, imf->name, img->w, img->h);
    } else {
	sprintf(shortbuf, "%s.%dx%d.", imf->name, img->w, img->h);
    }

    if (w == ims->mono_w) {
	strcat(shortbuf, "b");
	if (flag) {
	    if (img->rawmonodata && (fp = fopen(shortbuf, "w"))) {
		write_xbm_file(fp, imf->name, img->w, img->h, img->rawmonodata);
		ok = 1;
	    }
	} else {
	    if (read_xbm_file(shortbuf, imf, mark_changed)) {
		mark_changed(imf, 0);
		ok = 1;
	    }
	}
    } else if (w == ims->mask_w) {
	strcat(shortbuf, "m");
	if (flag) {
	    if (img->rawmaskdata && (fp = fopen(shortbuf, "w"))) {
		write_xbm_file(fp, imf->name, img->w, img->h, img->rawmaskdata);
		ok = 1;
	    }
	} else {
	    if (read_xbm_file(shortbuf, imf, mark_changed)) {
		mark_changed(imf, 0);
		ok = 1;
	    }
	}
    } else  if (w == ims->colr_w) {
	strcat(shortbuf, "xpm");
	if (img->rawcolrdata && (fp = fopen(shortbuf, "w"))) {
	    write_xpm_file(fp, imf->name, img);
	    ok = 1;
	} else {
	    if (read_xpm_file(shortbuf, imf, mark_changed)) {
		mark_changed(imf, 0);
		ok = 1;
	    }
	}
    } else {
	XBell(dpy,35);
	return 0;
    }

    if (fp)
      fclose(fp);

    if (ok && !flag) {
	imag = fms->imagn;
	undisplay_family(i);
	x11_interp_imf(dpy, rootwin, images[i], TRUE);
	img = best_image(images[i], basew, baseh);
	if (img->hook != NULL) {
	    X11Image *ximg = (X11Image *) img->hook;
	    if (ximg->colr != None) {
		XtVaSetValues(fms->image, XtNbitmap, ximg->colr, NULL);
	    } else if (ximg->mono != None) {
		XtVaSetValues(fms->image, XtNbitmap, ximg->mono, NULL);
	    } else {
		XtVaSetValues(fms->image, XtNbitmap, ximg->mask, NULL);
	    }
	}
	display_family(i, imag);
    }

    if (ok) {
	sprintf(buffer, "%s \"%s\"", flag ? "exported in" : "updated from",
		shortbuf);
    } else {
	XBell(dpy,35);
	sprintf(buffer, "%s failed", flag ? "export" : "update");
    }
    XtVaSetValues(fms->label, XtNlabel, buffer, NULL);
    return ok;
}

Image *
find_image(imf, w, h)
ImageFamily *imf;
int w, h;
{
    Image *img;

    for (img = imf->images; img != NULL; img = img->next) {
	if (w == img->w && h == img->h) {
	    return img;
	}
    }
    return NULL;
}

/* i>=0: delete family #i;  i<0: delete selected families */

void 
delete_family (i)
int i;
{
    int j, n, hidden;
    Boolean state;
    
    hidden = 0;
    n = 0;
    for (j=0; j<numfamilies; j++) {
	if (i<0) {
	    XtVaGetValues(family[j].name, XtNstate, &state, NULL);
	}
	if ((i<0 && state) || (j==i)) {
	    if (!hidden)  hide_image_families();
	    hidden = 1;
	    destroy_family(j);
	} else {
	    family[n] = family[j];
	    images[n] = images[j];
	    n++;
	}
    }
    
    if (numfamilies>n) {
	sprintf(buffer, "%d image families left", n);
	XtVaSetValues(message, XtNlabel, buffer, NULL);
    }
    numfamilies = numimages = n;
    if (hidden) {
	cols = MINCOLS;
	show_image_families(FALSE);
    }
}

void 
destroy_family (i)
int i;
{
    FamilyStuff *fms = &family[i];
    ImageStuff *ims;

    for (ims = fms->images; ims; ims = ims->next) {
	delete_image(ims->mono_w);
	delete_image(ims->mask_w);
	delete_image(ims->colr_w);
    }
    family[i].shell = NULL;
}

int
empty_family(i)
int i;
{
    Image *img;
    X11Image *ximg;

    for (img = images[i]->images; img != NULL; img = img->next) {
	if (!img->hook)
	  continue;
	ximg = (X11Image *) img->hook;
	if (ximg->mono != None || ximg->colr != None || ximg->mask != None)
	  return 0;
    }
    return 1;
}
