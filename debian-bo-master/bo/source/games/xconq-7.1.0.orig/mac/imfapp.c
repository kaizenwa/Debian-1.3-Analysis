/* Program that translates and previews image families for Mac Xconq.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This is a simple app whose function is to translate and preview
   Xconq images and image families. */

#include "config.h"
#include "misc.h"
void prealloc_debug(void);
#include "lisp.h"
#include "imf.h"

#ifdef THINK_C
#include <MacHeaders>
#else /* probably MPW */
#include <Types.h>
#include <Memory.h>
#include <Resources.h>
#include <QuickDraw.h>
#include <Values.h>
#include <Fonts.h>
#include <Events.h>
#include <Windows.h>
#include <Menus.h>
#include <Dialogs.h>
#include <Desk.h>
#include <ToolUtils.h>
#include <SegLoad.h>
#include <Files.h>
#include <Folders.h>
#include <OSUtils.h>
#include <OSEvents.h>
#include <DiskInit.h>
#include <Packages.h>
#include <Traps.h>
#include <StandardFile.h>
#endif

#include "macimf.h"

#ifdef MPW
#define QD(whatever) (qd.##whatever)
#define QDPat(whatever) (&(qd.##whatever))
#endif
#ifdef THINK_C
#define QD(whatever) (whatever)
#define QDPat(whatever) (whatever)
#endif

/* Definitions for the menus. */

#define mbMain 128

#define mApple 128

#define miAbout 1

#define mFile 129

#define miFileNew 1
#define miFileOpen 2
#define miFileAddImf 3
#define miFileAddResources 4
/* 5 */
#define miFileSave 6
#define miFileSaveImf 7
#define miFileSaveResources 8
#define miFileSelectedOnly 9
/* 10 */
#define miFileQuit 11

#define mEdit 130

#define miEditCut 1
#define miEditCopy 2
#define miEditPaste 3
#define miEditClear 4

#define mView 131
#define miView4x4 1
#define miView8x8 2
#define miView16x16 3
#define miView32x32 4
#define miView64x64 5
/* 6 */
#define miViewColor 7
#define miViewNames 8
#define miViewMask 9
/* 10 */
#define miViewAsUnit 11
#define miViewAsTerrain 12
#define miViewAsEmblem 13
#define miViewWithUnit 14
#define miViewWithTerrain 15
#define miViewWithEmblem 16
/* 17 */
#define miViewIcons 18
#define miViewTiles 19
#define miViewCQD 20

#define wImages 128

#define aAbout 128
#define aWarning 140
#define aError 141

#define is_selected(imf) (imf == selected_imf)

/* Global variables. */

int debug_output = 1;

/* Variables tweaked by menu items. */

int write_all = 1;

int show_color = 1;
int show_names = 1;
int show_mask = 1;

int vary_unit = 1;
int vary_terrain = 0;
int vary_emblem = 0;

int with_unit = 0;
int with_terrain = 0;
int with_emblem = 0;

int select_icons = 1;
int select_tiles = 1;

/* Pointers to the image families being held constant for display. */

ImageFamily *const_terrain = NULL;

ImageFamily *const_unit = NULL;

ImageFamily *const_emblem = NULL;

int iw = 16, ih = 16;

int hasColorQD;

int useColorQD;

int useWNE;

char spbuf[1000];
char tmpbuf[1000];

Str255 tmpstr;

/* Resource ids. */

short nextimageid;
short nextpatid;
short nextppatid;
short nextsicnid;
short nexticonid;
short nextcicnid;
short nextcolorid;
short nextimfid;

/* Pointer to storage for all the image families. */

ImageFamily *selected_imf = NULL;

int selected_n = 0;

WindowPtr imagewin = NULL;

Rect dragrect = { -32000, -32000, 32000, 32000 };
Rect sizerect;

/* This variable leaves space for two lines of text at the top of the window. */

int toplineh = 32;

/* The one scrollbar. */

ControlHandle vscrollbar = nil;

Rect vscrollrect;

int sbarwid = 15;

int numvisrows = 0;

int firstvisrow = 0;

int winwidth, winheight;

/* The numbers of rows and columns of images. */

int rows, cols;

/* The size of the rectangle allocated to a single image. */

int eltw, elth;

CursHandle watchcursor;

/* Function prototypes. */

void do_event(EventRecord *event);
void grow_scrollbar(void);
void do_menu_command(long which);
void adjust_menus(void);
void clear_everything(void);
pascal void scroll_proc(ControlHandle control, short code);
void handle_mouse_click(Point mouse, int mods);
void force_update(void);

void imf_callback(ImageFamily *imf, int loadnow);

void open_imf_dir_file(void);
void open_imf_file(void);
void open_resource_file(void);
void collect_all_resources(ResType typ);
void collect_all_colors(ResType typ);

void save_imf_dir_file(void);
void save_imf_file(void);
void save_resource_file(void);
void make_imf_resources(ImageFamily *imf);
void make_imc_resources(ImageColor *imc);

void update_image_window(void);
void draw_topline(void);
void draw_one_image(ImageFamily *imf, int col, int row);
void set_scrollbar(void);
void invert_selected_imf(void);
void draw_as_terrain_image(int sx, int sy, int sw, int sh, ImageFamily *imf);
void draw_as_unit_image(WindowPtr win, int sx, int sy, int sw, int sh, ImageFamily *imf);
void draw_as_emblem_image(WindowPtr win, int ex, int ey, int ew, int eh, ImageFamily *imf);

char *copy_pascal_string(char *str);

/* The program proper. */

int
main()
{
	SysEnvRec se;
	Handle menubar;
	MenuHandle menu;
	RgnHandle cursorrgn;
	Boolean gotevent;
	EventRecord	event;

	InitGraf(&QD(thePort));
	InitFonts();
	FlushEvents(everyEvent, 0);
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(NULL);
	InitCursor();
	watchcursor = GetCursor(watchCursor);

	SysEnvirons(2, &se);
	hasColorQD = se.hasColorQD;
	useColorQD = hasColorQD;
	/* Set up the menu bar.  No trickery needed. */
	menubar = GetNewMBar(mbMain);
	SetMenuBar(menubar);
	/* Add the DAs etc as usual. */
	menu = GetMHandle(mApple);
	if (menu != nil) {
		AddResMenu(menu, 'DRVR');
	}
	DrawMenuBar();

	init_lisp();

	/* Create the main window we're going to play in. */
	if (hasColorQD) {
		imagewin = GetNewCWindow(wImages, NULL, (WindowPtr) -1L);
	} else {
		imagewin = GetNewWindow(wImages, NULL, (WindowPtr) -1L);
	}
	vscrollrect = imagewin->portRect;
	vscrollrect.left = vscrollrect.right - sbarwid;  vscrollrect.top += toplineh - 1;
	vscrollrect.right += 1;  vscrollrect.bottom -= sbarwid - 1;
	vscrollbar =
		NewControl(imagewin, &vscrollrect, "\p", 1, 0, 0, 100, scrollBarProc, 0L);
	ShowWindow(imagewin);

	sizerect.top = 50;
	sizerect.left = 50;
	sizerect.bottom = QD(screenBits).bounds.bottom - QD(screenBits).bounds.top;
	sizerect.right  = QD(screenBits).bounds.right  - QD(screenBits).bounds.left;

	useWNE = (NGetTrapAddress(0x60, ToolTrap) != NGetTrapAddress(0x9f, ToolTrap));
	/* Pass WNE an empty region the 1st time thru. */
	cursorrgn = NewRgn();
	while (1) {
		/* Use WaitNextEvent if it is available. */
		if (useWNE) {
			gotevent = WaitNextEvent(everyEvent, &event, 0L, cursorrgn);
		} else {
			SystemTask();
			gotevent = GetNextEvent(everyEvent, &event);
		}
		if (gotevent) {
			do_event(&event);
		}
	}
	return 0;
}

/* Given an event, figure out what to do with it. */

void
do_event(event)
EventRecord *event;
{
	short part, err;
	WindowPtr window;
	char key;
	Point pnt;
	long winsize;
	GrafPtr oldport;

	switch (event->what) {
		case mouseDown:
			part = FindWindow(event->where, &window);
			switch (part) {
				case inMenuBar:
					adjust_menus();
					do_menu_command(MenuSelect(event->where));
					break;
				case inSysWindow:
					SystemClick(event, window);
					break;
				case inContent:
					if (window != FrontWindow()) {
						SelectWindow(window);
						/* We just want to discard the event, since clicks in a
						   windows are sometimes irreversible actions. */
						adjust_menus();
					} else {
						handle_mouse_click(event->where, event->modifiers);
					}
					break;
				case inDrag:
					DragWindow(window, event->where, &dragrect);
					break;
				case inGrow:
					winsize = GrowWindow(window, event->where, &sizerect);
					if (winsize != 0) {
						GetPort(&oldport);
						SetPort(window);
						EraseRect(&window->portRect);
						SizeWindow(window, LoWord(winsize), HiWord(winsize), 1);
						grow_scrollbar();
						InvalRect(&window->portRect);
						SetPort(oldport);
					}
					break;
				case inZoomIn:
				case inZoomOut:
					if (TrackBox(window, event->where, part)) {
						GetPort(&oldport);
						/* The window must be the current port. (ZoomWindow bug) */
						SetPort(window);
						EraseRect(&window->portRect);
						ZoomWindow(window, part, true);
						grow_scrollbar();
						InvalRect(&window->portRect);
						SetPort(oldport);
					}
					break;
				case inGoAway:
					/* Don't mess around, just shut down. */
					ExitToShell();
					break;
			}
			break;
		case mouseUp:
			part = FindWindow(event->where, &window);
			switch (part) {
				case inContent:
					if (0 /* up in diff window than down? */) {
					} else {
					}
					break;
			}
			break;
		case keyDown:
		case autoKey:
			key = event->message & charCodeMask;
			/* Check for menukey equivalents. */
			if (event->modifiers & cmdKey) {
				if (event->what == keyDown) {
					adjust_menus();
					do_menu_command(MenuKey(key));
				}
			} else {
				if (event->what == keyDown) {
					/* Random keypress, interpret it. */
				}
			}
			break;
		case activateEvt:
			break;
		case updateEvt:
			if (imagewin == ((WindowPtr) event->message)) {
				update_image_window();
			}
			break;
		case diskEvt:
			/*	Call DIBadMount in response to a diskEvt, so that the user can format
				 a floppy. (from DTS Sample) */
			if (HiWord(event->message) != noErr) {
				SetPt(&pnt, 50, 50);
				err = DIBadMount(pnt, event->message);
			}
			break;
#if 0
		case kOSEvent:
		/*	1.02 - must BitAND with 0x0FF to get only low byte */
			switch ((event->message >> 24) & 0x0FF) {		/* high byte of message */
				case kSuspendResumeMessage:		/* suspend/resume is also an activate/deactivate */
					gInBackground = (event->message & kResumeMask) == 0;
					DoActivate(FrontWindow(), !gInBackground);
					break;
			}
			break;
#endif
		default:
			break;
	}
}

void
grow_scrollbar()
{
	Rect tmprect = imagewin->portRect;

	MoveControl(vscrollbar, tmprect.right - sbarwid, toplineh - 1);
	SizeControl(vscrollbar, sbarwid + 1, tmprect.bottom - tmprect.top - toplineh - sbarwid + 1 + 1);
}

/* Decipher and do a menu command. */

void
do_menu_command(which)
long which;
{
	short menuid, menuitem;
	Str255 daname;
	short darefnum;

	menuid = HiWord(which);
	menuitem = LoWord(which);
	switch (menuid) {
		case mApple:
			switch (menuitem) {
				case miAbout:
					Alert(aAbout, nil);
					break;
				default:
					GetItem(GetMHandle(mApple), menuitem, daname);
					darefnum = OpenDeskAcc(daname);
			}
			break;
		case mFile:
			switch (menuitem) {
				case miFileNew:
					clear_everything();
					/* Clean up the display (should not be necessary though). */
					force_update();
					break;
				case miFileOpen:
					open_imf_dir_file();
					/* Clean up the display (should not be necessary though). */
					force_update();
					break;
				case miFileAddImf:
					open_imf_file();
					/* Clean up the display (should not be necessary though). */
					force_update();
					break;
				case miFileAddResources:
					open_resource_file();
					/* Clean up the display (should not be necessary though). */
					force_update();
					break;
				case miFileSave:
					save_imf_dir_file();
					break;
				case miFileSaveImf:
					save_imf_file();
					break;
				case miFileSaveResources:
					save_resource_file();
					break;
				case miFileSelectedOnly:
					write_all = !write_all;
					break;
				case miFileQuit:
					ExitToShell();
					break;
			}
			break;
		case mEdit:
			/* handledbyda = SystemEdit(menuitem-1); */
			switch (menuitem)  {
				case miEditCut:
					break;
				case miEditCopy:
					break;
				case miEditPaste:
					break;
				case miEditClear:
					break;
			}
			break;
		case mView:
			switch (menuitem) {
				case miView4x4:
					iw = ih = 4;
					break;
				case miView8x8:
					iw = ih = 8;
					break;
				case miView16x16:
					iw = ih = 16;
					break;
				case miView32x32:
					iw = ih = 32;
					break;
				case miView64x64:
					iw = ih = 64;
					break;
				case miViewColor:
					show_color = !show_color;
					break;
				case miViewNames:
					show_names = !show_names;
					break;
				case miViewMask:
					show_mask = !show_mask;
					break;
				case miViewAsUnit:
					vary_unit = 1;
					vary_terrain = 0;
					vary_emblem = 0;
					break;
				case miViewAsTerrain:
					vary_unit = 0;
					vary_terrain = 1;
					vary_emblem = 0;
					break;
				case miViewAsEmblem:
					vary_unit = 0;
					vary_terrain = 0;
					vary_emblem = 1;
					break;
				case miViewWithUnit:
					with_unit = !with_unit;
					const_unit = selected_imf;
					break;
				case miViewWithTerrain:
					with_terrain = !with_terrain;
					const_terrain = selected_imf;
					break;
				case miViewWithEmblem:
					with_emblem = !with_emblem;
					const_emblem = selected_imf;
					break;
				case miViewIcons:
					select_icons = !select_icons;
					break;
				case miViewTiles:
					select_tiles = !select_tiles;
					break;
				case miViewCQD:
					useColorQD = !useColorQD;
					break;
			}
			force_update();
			break;
	}
	HiliteMenu(0);
}

/* Set enabling and decoration of menus to reflect current state. */

void
adjust_menus()
{
	MenuHandle menu;

	if ((menu = GetMHandle(mFile)) != nil) {
		CheckItem(menu, miFileSelectedOnly, !write_all);
	}
	if ((menu = GetMHandle(mView)) != nil) {
		CheckItem(menu, miView4x4, (iw == 4));
		CheckItem(menu, miView8x8, (iw == 8));
		CheckItem(menu, miView16x16, (iw == 16));
		CheckItem(menu, miView32x32, (iw == 32));
		CheckItem(menu, miView64x64, (iw == 64));
		if (hasColorQD) {
			EnableItem(menu, miViewColor);
			CheckItem(menu, miViewColor, show_color);
			EnableItem(menu, miViewCQD);
			CheckItem(menu, miViewCQD, useColorQD);
		} else {
			DisableItem(menu, miViewColor);
			DisableItem(menu, miViewCQD);
		}
		CheckItem(menu, miViewNames, show_names);
		CheckItem(menu, miViewMask, show_mask);
		CheckItem(menu, miViewAsUnit, vary_unit);
		CheckItem(menu, miViewAsTerrain, vary_terrain);
		CheckItem(menu, miViewAsEmblem, vary_emblem);
		CheckItem(menu, miViewWithUnit, with_unit);
		if ((selected_imf != NULL && !vary_unit) || with_unit) {
			EnableItem(menu, miViewWithUnit);
		} else {
			DisableItem(menu, miViewWithUnit);
		}
		CheckItem(menu, miViewWithTerrain, with_terrain);
		if ((selected_imf != NULL && !vary_terrain) || with_terrain) {
			EnableItem(menu, miViewWithTerrain);
		} else {
			DisableItem(menu, miViewWithTerrain);
		}
		CheckItem(menu, miViewWithEmblem, with_emblem);
		if ((selected_imf != NULL && !vary_emblem) || with_emblem) {
			EnableItem(menu, miViewWithEmblem);
		} else {
			DisableItem(menu, miViewWithEmblem);
		}
		CheckItem(menu, miViewIcons, select_icons);
		CheckItem(menu, miViewTiles, select_tiles);
		CheckItem(menu, miViewCQD, useColorQD);
	}
}

/* Empty all data, files, etc. */

void
clear_everything()
{
	numimages = 0;
	numcolors = 0;

	selected_imf = NULL;
	selected_n = 0;
	const_terrain = NULL;
	const_unit = NULL;
	const_emblem = NULL;
	numvisrows = firstvisrow = 0;
}

pascal void
scroll_proc(control, code)
ControlHandle control;
short code;
{
	int curvalue, jump;

	curvalue = GetCtlValue(control);
	switch (code) {
		case inPageDown:
			jump = (numvisrows > 2 ? numvisrows - 2 : 1);
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = - (numvisrows > 2 ? numvisrows - 2 : 1);
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	curvalue += jump;
	SetCtlValue(control, curvalue);
}

void
handle_mouse_click(mouse, mods)
Point mouse;
int mods;
{
	int row, col, n;
	ControlHandle control;
	short part, oldvalue;

	SetPort(imagewin);
	GlobalToLocal(&mouse);
	part = FindControl(mouse, imagewin, &control);
	if (control == vscrollbar) {
		oldvalue = GetCtlValue(vscrollbar);
		switch (part) {
			case inThumb:
				part = TrackControl(control, mouse, NULL);
				break;
			default:
				part = TrackControl(control, mouse, (ProcPtr) scroll_proc);
				break;
		}
		firstvisrow = GetCtlValue(vscrollbar);
		if (oldvalue != firstvisrow) {
			force_update();
		}
	} else if (mouse.v <= toplineh) {
		/* Clicking in the topline area de-selects always. */
		invert_selected_imf();
		selected_imf = NULL;
		draw_topline();
	} else {
		/* Figure out which image was clicked on. */
		col = mouse.h / eltw;
		row = (mouse.v - toplineh) / elth + firstvisrow;
		n = row * cols + col;
		if (n >= 0 && n < numimages) {
			if (!is_selected(images[n])) {
				invert_selected_imf();
				selected_imf = images[n];
				selected_n = n;
				invert_selected_imf();
			}
			if (mods & cmdKey) {
				with_terrain = 1;
				const_terrain = selected_imf;
				force_update();
			}
			if (mods & optionKey) {
				with_emblem = 1;
				const_emblem = selected_imf;
				force_update();
			}
		} else {
			invert_selected_imf();
			selected_imf = NULL;
		}
		draw_topline();
	}
}

void
force_update()
{
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(imagewin);
	EraseRect(&imagewin->portRect);
	InvalRect(&imagewin->portRect);
	SetPort(oldport);
}

/* INPUT/OUTPUT */

void
imf_callback(imf, loadnow)
ImageFamily *imf;
int loadnow;
{
	if (loadnow && imf != NULL)
	  mac_interp_imf(imf);
	/* Do some visual feedback periodically. */
	if (numimages % 10 == 0)
	  draw_topline();
}

void
open_imf_dir_file()
{
	char filename[BUFSIZE];
	int startlineno = 0, endlineno = 0;
	FILE *fp;
	Point pnt;
	SFTypeList typelist;
	SFReply reply;

	/* Gotta be somewhere... */
	SetPt(&pnt, 100, 100);
	/* Only read text files. */
	typelist[0] = 'TEXT';
	SFGetFile(pnt, "\p", NULL, 1, typelist, NULL, &reply);
	if (reply.good) {
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		p2c(((char *) reply.fName), filename);
		SetCursor(*watchcursor);
		fp = fopen(filename, "r");
		if (fp != NULL) {
			load_image_families(fp, TRUE, imf_callback);
			fclose(fp);
		}
		/* Sort all into alphabetical order. */
		sort_all_images();
		sort_all_colors();
		SetCursor(&QD(arrow));
	}
}

/* Open and read/interpret the contents of an imf file. */

void
open_imf_file()
{
	char filename[BUFSIZE];
	Point pnt;
	SFTypeList typelist;
	SFReply reply;

	/* Gotta be somewhere... */
	SetPt(&pnt, 100, 100);
	/* Only read text files. */
	typelist[0] = 'TEXT';
	SFGetFile(pnt, "\p", NULL, 1, typelist, NULL, &reply);
	if (reply.good) {
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		p2c(((char *) reply.fName), filename);
		SetCursor(*watchcursor);
		load_imf_file(filename, imf_callback);
		/* Sort all into alphabetical order. */
		sort_all_images();
		sort_all_colors();
		SetCursor(&QD(arrow));
	}
}

/* Get the name of a resource file, open it, and make image families out
   of all appropriate resource types. */

void
open_resource_file()
{
	Point pnt;
	SFTypeList typelist;
	SFReply reply;

	/* Gotta be somewhere... */
	SetPt(&pnt, 100, 100);
	/* Pick up any sort of file. */
	SFGetFile(pnt, "\p", NULL, -1, typelist, NULL, &reply);
	if (reply.good) {
		SetVol(reply.fName, reply.vRefNum);
		SetCursor(*watchcursor);
		if (OpenResFile(reply.fName) != -1) {
			/* Now that all the resources are available, go through all types of rsrcs
			   that might have useful images. */
			collect_all_resources('XCif');
			collect_all_resources('cicn');
			collect_all_resources('ICON');
			collect_all_resources('SICN');
			collect_all_resources('ppat');
			collect_all_resources('PAT ');
			collect_all_colors('XCic');
			/* Sort all into alphabetical order. */
			sort_all_images();
			sort_all_colors();
		} else {
			init_warning("could not open resource file");
		}
		SetCursor(&QD(arrow));
	}
}

/* Given a resource type, get image families for all resources of that type. */

void
collect_all_resources(ResType typ)
{
    int i, n, len;
	char *imfname;
	ImageFamily *imf;
	Handle handle;
	short resid;  ResType restype;  Str255 resname;

	n = CountResources(typ);
	for (i = 0; i < n; ++i ) {
		imfname = NULL;
		handle = GetIndResource(typ, i + 1);
		GetResInfo(handle, &resid, &restype, resname);
		if (resname[0] > 0) {
			imfname = copy_pascal_string((char *) resname);
		} else {
			/* (should synth names for unnamed resources) */
		}
		len = strlen(imfname);
		if (imfname != NULL
			/* Resources with names ending in " mask" are masks for other resources,
			   so ignore them here. */
		    && strcmp(imfname + len - 5, " mask") != 0) {
		    /* Peel off size specs if present. */
		    if (strcmp(imfname + len - 4, " 8x8") == 0) {
				/* String is a copy, so we can do this. */
				imfname[len - 4] = '\0';
		    }
		    if (strcmp(imfname + len - 6, " 16x16") == 0
				|| strcmp(imfname + len - 6, " 32x32") == 0) {
				/* String is a copy, so we can do this. */
				imfname[len - 6] = '\0';
		    }
			imf = get_imf(imfname);
			mac_load_imf(imf);
			make_generic_image_data(imf);
			check_imf(imf);
			/* Do some visual feedback periodically. */
			if (numimages % 10 == 0)
			  draw_topline();
		}
	}
}

/* Given a resource type, get named colors for all resources of that type. */

void
collect_all_colors(ResType typ)
{
    int i, n;
	char *imcname;
	ImageColor *imc;
	Handle handle;
	short resid;  ResType restype;  Str255 resname;

	n = CountResources(typ);
	for (i = 0; i < n; ++i ) {
		imcname = NULL;
		handle = GetIndResource(typ, i + 1);
		GetResInfo(handle, &resid, &restype, resname);
		if (resname[0] > 0) {
			imcname = copy_pascal_string((char *) resname);
		} else {
			/* (should synth names for unnamed resources) */
		}
		if (imcname != NULL) {
			imc = get_imc(imcname);
			mac_load_image_color(imc);
			/* Do some visual feedback periodically. */
			if (numcolors % 10 == 0)
			  draw_topline();
		}
	}
}

void
save_imf_dir_file()
{
    int i;
    char filename[BUFSIZE], *loc;
    Point pnt;
    FILE *fp;
    SFReply reply;
	ImageFamily *imf;

	SetPt(&pnt, 100, 100);
	SFPutFile(pnt, "\p", "\pimf.dir", nil, &reply);
	if (reply.good) {
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		p2c(((char *) reply.fName), filename);
		SetCursor(*watchcursor);
		/* (should genericize into a write_imf_dir that takes array of images) */
		fp = fopen(filename, "w");
		if (fp != NULL) {
			fprintf(fp, "ImageFamilyName FileName\n");
			for (i = 0; i < numimages; ++i) {
				imf = images[i];
				if (write_all || is_selected(imf)) {
					loc = "???";
					if (imf->location && !empty_string(imf->location->name))
					  loc = imf->location->name;
					fprintf(fp, "%s %s\n", imf->name, loc);
				}
				/* (to write imf files, should scan through images once for
				    each file, writing all images found that are in that file) */
			}
			fprintf(fp, ". .\n");
			fclose(fp);
		} else {
			run_warning("could not open file for writing");
		}
		SetCursor(&QD(arrow));
	}
}

/* Write all the images into an imf file. */

void
save_imf_file()
{
    int i;
    char filename[BUFSIZE];
    Point pnt;
    FILE *fp;
    SFReply reply;

	SetPt(&pnt, 100, 100);
	SFPutFile(pnt, "\p", "\pimages.imf", nil, &reply);
	if (reply.good) {
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		p2c(((char *) reply.fName), filename);
		SetCursor(*watchcursor);
		fp = fopen(filename, "w");
		if (fp != NULL) {
			/* Write out the imf forms of all the image families. */
			for (i = 0; i < numimages; ++i) {
				if (write_all || is_selected(images[i])) {
					make_generic_image_data(images[i]);
					write_imf(fp, images[i]);
				}
			}
			/* Write out the image colors. */
			for (i = 0; i < numcolors; ++i) {
				if (write_all) {  /* (should have a way to select colors) */
					write_imc(fp, colors[i]);
				}
			}
			fclose(fp);
		} else {
			run_warning("could not open file for writing");
		}
		SetCursor(&QD(arrow));
	}
}

/* Write all the images, as resources, into a resource file. */

void
save_resource_file()
{
	int refnum, i;
	Point pnt;
    SFReply reply;

	SetPt(&pnt, 100, 100);
	SFPutFile(pnt, "\p", "\pimages.imf Images", nil, &reply);
	if (reply.good) {
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		SetCursor(*watchcursor);
		CreateResFile(reply.fName);
		refnum = OpenResFile(reply.fName);
		if (refnum >= 0) {
			/* All synthesized resource ids should go from 1000 up. */
			nextpatid = 1000;
			nextppatid = 1000;
			nextsicnid = 1000;
			nexticonid = 1000;
			nextcicnid = 1000;
			nextcolorid = 1000;
			nextimfid = 1000;
			/* Make resources for named colors. */
			for (i = 0; i < numcolors; ++i) {
				make_imc_resources(colors[i]);
			}
			/* Make resources for image family specifications. */
			for (i = 0; i < numimages; ++i) {
				if (write_all || is_selected(images[i])) {
					make_imf_resources(images[i]);
				}
			}
			/* Closing the resource file causes actual writing. */
			CloseResFile(refnum);
		} else {
			run_warning("could not open resource file");
		}
		SetCursor(&QD(arrow));
	}
}

static int write_location = FALSE;

void
make_imf_resources(imf)
ImageFamily *imf;
{
	int j, makexcif = FALSE, needimghead;
	char buf[BUFSIZE], xcifbuf[1000];
	Handle imfhandle, handle;
	Str255 tmpstr, tmpstrmask;
	Image *img;
	MacImage *macimg;

	if (imf == NULL || imf->name == NULL)
	  return;
	/* Make the resource names that we will use. */
	c2p(imf->name, tmpstr);
	sprintf(buf, "%s mask", imf->name);
	c2p(buf, tmpstrmask);
	sprintf(xcifbuf, "(");
	if (write_location && imf->location && imf->location->name) {
		sprintf(xcifbuf+strlen(xcifbuf), "(in \"%s\")", imf->location->name);
		makexcif = TRUE;
	}
    for (img = imf->images; img != NULL; img = img->next) {
		needimghead = 1;
		if (img->istile || img->embedname) {
			sprintf(xcifbuf+strlen(xcifbuf), " ((%d %d", img->w, img->h);
			if (img->istile)
			  strcat(xcifbuf, " tile");
			strcat(xcifbuf, ")");
			needimghead = 0;
			if (img->embedname) {
		    	sprintf(xcifbuf+strlen(xcifbuf), " (embed \"%s\")", img->embedname);
		    }
		    makexcif = TRUE;
		}
		if (!needimghead)
		  strcat(xcifbuf, ")");
		macimg = get_mac_image(img);
		/* Now make Mac-specific resources for image cases that we know about. */
		if (macimg->patdefined) {
			handle = NewHandle(8);
			for (j = 0; j < 8; ++j) {
				(*handle)[j] = macimg->monopat[j];
			}
			AddResource(handle, 'PAT ', nextpatid++, tmpstr);
		}
		if (macimg->monosicn != nil) {
			if (macimg->masksicn != nil) {
				handle = NewHandle(64);
				/* Add the mask as a second sicn. */
				for (j = 0; j < 32; ++j)
				  (*handle)[j+32] = (*(macimg->masksicn))[j];
			} else {
				handle = NewHandle(32);
			}
			for (j = 0; j < 32; ++j)
			  (*handle)[j] = (*(macimg->monosicn))[j];
			AddResource(handle, 'SICN', nextsicnid++, tmpstr);
		} else if (macimg->masksicn != nil) {
			/* A weird case, but don't lose it. */
			handle = NewHandle(32);
			for (j = 0; j < 32; ++j)
			  (*handle)[j] = (*(macimg->masksicn))[j];
			AddResource(handle, 'SICN', nextsicnid++, tmpstrmask);
		}
		if (macimg->monoicon != nil) {
			handle = NewHandle(128);
			for (j = 0; j < 128; ++j)
			  (*handle)[j] = (*(macimg->monoicon))[j];
			AddResource(handle, 'ICON', nexticonid++, tmpstr);
		}
		if (macimg->maskicon != nil) {
			handle = NewHandle(128);
			for (j = 0; j < 128; ++j)
			  (*handle)[j] = (*(macimg->maskicon))[j];
			AddResource(handle, 'ICON', nexticonid++, tmpstrmask);
		}
		if (hasColorQD && macimg->colrpat != nil) {
			int patsize, h, pixmapsize, ctabsize, i;
			char *addr;
			Rect bounds;
			PixPatHandle pixpathandle;
			PixPat tmppixpat;
			PixMapHandle pmhandle;
			PixMap tmppixmap;
			CTabHandle ctabhandle;

			pixpathandle = macimg->colrpat;
			pmhandle = (*pixpathandle)->patMap;
			bounds = (*pmhandle)->bounds;
			h = bounds.bottom - bounds.top;
			pixmapsize = h * ((*pmhandle)->rowBytes & 0x3fff);
			ctabhandle = (*pmhandle)->pmTable;
			ctabsize = 8 + 8 * ((*ctabhandle)->ctSize + 1);
			/* Make a copy of the pixpat and bash its pointers/handles. */
 			memcpy(&tmppixpat, *(pixpathandle), sizeof(PixPat));
 			tmppixpat.patMap = (PixMapHandle) (sizeof(PixPat));
 			tmppixpat.patData = (Handle) (sizeof(PixPat) + sizeof(PixMap));
			tmppixpat.patXData = 0;
			tmppixpat.patXValid = -1;
			tmppixpat.patXMap = 0;
			if (macimg->patdefined) {
				for (i = 0; i < 8; ++i)
				  ((char *) &tmppixpat.pat1Data)[i] = ((char *) &(macimg->monopat))[i];
			} else {
				for (i = 0; i < 8; ++i)
				  ((char *) &tmppixpat.pat1Data)[i] = 0;
			}
			/* Make a copy of the pixmap and bash it. */
			memcpy(&tmppixmap, *pmhandle, sizeof(PixMap));
			tmppixmap.rowBytes |= 0x8000;
			tmppixmap.pmTable = (CTabHandle) (sizeof(PixPat) + sizeof(PixMap) + pixmapsize);
			/* Now allocate a handle for the resource and fill it in. */
			patsize = sizeof(PixPat) + sizeof(PixMap) + pixmapsize + ctabsize;
			handle = NewHandle(patsize);
			/* Fill up the handle. */
			HLock(handle);
			addr = *handle;
			memset(addr, 0, patsize);
			memcpy(addr, &tmppixpat, sizeof(PixPat));
			addr += sizeof(PixPat);
			memcpy(addr, &tmppixmap, sizeof(PixMap));
			addr += sizeof(PixMap);
			memcpy(addr, *((*pixpathandle)->patData), pixmapsize);
			addr += pixmapsize;
			memcpy(addr, *ctabhandle, ctabsize);
			HUnlock(handle);
			AddResource(handle, 'ppat', nextppatid++, tmpstr);
		}
		if (hasColorQD && macimg->colricon != nil) {
			int cicnsize, h, pixmapsize, masksize, monosize, ctabsize, zero = 0;
			char *addr;
			Rect bounds;
			CIconHandle cicnhandle;
			PixMap tmppixmap;
			BitMap tmpmaskbitmap;
			BitMap tmpmonobitmap;
			CTabHandle ctabhandle;

			cicnhandle = (CIconHandle) macimg->colricon;
			bounds = (*cicnhandle)->iconPMap.bounds;
			h = bounds.bottom - bounds.top;
			pixmapsize = h * ((*cicnhandle)->iconPMap.rowBytes & 0x3fff);
			ctabhandle = (*cicnhandle)->iconPMap.pmTable;
			ctabsize = 8 + 8 * ((*ctabhandle)->ctSize + 1);
			bounds = (*cicnhandle)->iconMask.bounds;
			h = bounds.bottom - bounds.top;
			masksize = h * (*cicnhandle)->iconMask.rowBytes;
			monosize = h * (*cicnhandle)->iconMask.rowBytes;
			/* Make a copy of the pixmap and bash it. */
			memcpy(&tmppixmap, &((*cicnhandle)->iconPMap), sizeof(PixMap));
			tmppixmap.baseAddr = 0;
			tmppixmap.rowBytes |= 0x8000;
			tmppixmap.pmTable = (CTabHandle) (sizeof(PixMap) + 2 * sizeof(BitMap) + 4
					   + masksize + monosize);
			/* Make a copy of the mask bitmap and bash it. */
			memcpy(&tmpmaskbitmap, &((*cicnhandle)->iconMask), sizeof(BitMap));
			tmpmaskbitmap.baseAddr = 0;
			/* Make a copy of the mono bitmap and bash it. */
			memcpy(&tmpmonobitmap, &((*cicnhandle)->iconBMap), sizeof(BitMap));
			tmpmonobitmap.baseAddr = 0;
			/* Now allocate a handle for the resource and fill it in. */
			cicnsize = sizeof(PixMap) + 2 * sizeof(BitMap) + 4
					   + masksize + monosize + ctabsize + pixmapsize;
			handle = NewHandle(cicnsize);
			/* Fill up the handle. */
			HLock(handle);
			addr = *handle;
			memset(addr, 0, cicnsize);
			memcpy(addr, &tmppixmap, sizeof(PixMap));
			addr += sizeof(PixMap);
			memcpy(addr, &tmpmaskbitmap, sizeof(BitMap));
			addr += sizeof(BitMap);
			memcpy(addr, &tmpmonobitmap, sizeof(BitMap));
			addr += sizeof(BitMap);
			memcpy(addr, &zero, 4);
			addr += 4;
			memcpy(addr, (char *) (*cicnhandle)->iconMaskData, masksize);
			addr += masksize;
			memcpy(addr, ((char *) (*cicnhandle)->iconMaskData) + masksize, monosize);
			addr += monosize;
			memcpy(addr, *ctabhandle, ctabsize);
			addr += ctabsize;
			memcpy(addr, *((*cicnhandle)->iconData), pixmapsize);
			HUnlock(handle);
			AddResource(handle, 'cicn', nextcicnid++, tmpstr);
		}
	}
	/* Close out the XCif data and make it into a resource too. */
	strcat(xcifbuf, ")");
	if (makexcif) {
		imfhandle = NewHandle(strlen(xcifbuf)+1);
		strcpy(*imfhandle, xcifbuf);
		AddResource(imfhandle, 'XCif', nextimfid++, tmpstr);
	}
}

void
make_imc_resources(imc)
ImageColor *imc;
{
	Handle handle;

	handle = NewHandle(6);
	((short *) (*handle))[0] = imc->r;
	((short *) (*handle))[1] = imc->g;
	((short *) (*handle))[2] = imc->b;
	c2p(imc->name, tmpstr);
	AddResource(handle, 'XCic', nextcolorid++, tmpstr);
}

/* GRAPHICS */

/* Totally redraw the one window. */

void
update_image_window()
{
	int row, col, n;
	GrafPtr oldport;

	BeginUpdate(imagewin);
	GetPort(&oldport);
	SetPort(imagewin);
	EraseRect(&(imagewin->portRect));
	draw_topline();
	eltw = (show_names ? 100 : iw + 4);  elth = ih + 4;
	/* (should be determined by choice of app font) */
	if (elth < 10) elth = 10;
	winwidth = imagewin->portRect.right - imagewin->portRect.left;
	winheight = imagewin->portRect.bottom - imagewin->portRect.top;
	if (with_terrain && const_terrain) {
		if (best_image(const_terrain, iw, ih)->istile) {
			draw_as_terrain_image(0, toplineh,
								  winwidth - sbarwid, winheight - toplineh - sbarwid,
								  const_terrain);
		} else {
			/* (should draw under each unit separately) */
		}
	}
	/* Compute how many columns we can fit, rounding down but always at least 1. */
	cols = winwidth / eltw;
	if (cols <= 0) cols = 1;
	/* We can get a little wider spacing by recalculating the element width. */
	eltw = (winwidth - 10) / cols;
	rows = numimages / cols + 1;
	numvisrows = (winheight - toplineh - 15) / elth;
	if (numvisrows > rows)
	  numvisrows = rows;
	if (firstvisrow + numvisrows > rows)
	  firstvisrow = rows - numvisrows;
	for (row = firstvisrow; row < (firstvisrow + numvisrows); ++row) {
		for (col = 0; col < cols; ++col) {
			n = row * cols + col;
			if (n >= numimages)
			  break;
			draw_one_image(images[n], col, row);
		}
	}
	invert_selected_imf();
	set_scrollbar();
	DrawControls(imagewin);
	DrawGrowIcon(imagewin);
	SetPort(oldport);
	EndUpdate(imagewin);
}

void
draw_topline()
{
	int first;
	Image *img;
	Rect tmprect;
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(imagewin);
	tmprect = imagewin->portRect;
	tmprect.bottom = tmprect.top + toplineh;
	EraseRect(&tmprect);
	TextFont(monaco);
	TextSize(9);
	sprintf(spbuf, "%d images", numimages);
	if (with_terrain && const_terrain != NULL) {
		sprintf(spbuf+strlen(spbuf), " (on %s terrain)", const_terrain->name);
	}
	if (with_emblem && const_emblem != NULL) {
		sprintf(spbuf+strlen(spbuf), " (with %s emblem)", const_emblem->name);
	}
	if (numcolors > 0) {
		sprintf(spbuf+strlen(spbuf), ", %d colors", numcolors);
	}
	c2p(spbuf, tmpstr);
	MoveTo(4, 12);
	DrawString(tmpstr);
	/* Draw a second line describing the selection. */
	if (selected_imf != NULL) {
		strcpy(spbuf, "[");
		strcat(spbuf, selected_imf->name);
		if (selected_imf->location && selected_imf->location->name)
		  sprintf(spbuf+strlen(spbuf), " (in \"%s\")", selected_imf->location->name);
		switch (selected_imf->numsizes) {
			case 0:
				strcat(spbuf, " (no images)");
				break;
			case 1:
				sprintf(spbuf+strlen(spbuf), " (1 size: %dx%d)",
						selected_imf->images->w, selected_imf->images->h);
				break;
			default:
				sprintf(spbuf+strlen(spbuf), " (%d sizes:", selected_imf->numsizes);
				first = TRUE;
				for (img = selected_imf->images; img != NULL; img = img->next) {
					if (first)
					  first = FALSE;
					else
					  strcat(spbuf, ",");
					sprintf(spbuf+strlen(spbuf), " %dx%d", img->w, img->h);
				}
				strcat(spbuf, ")");
				break;
		}
		if (selected_imf->notes != lispnil) {
			if (stringp(selected_imf->notes)) {
				strcat(spbuf, " ");
				strcat(spbuf, c_string(selected_imf->notes));
			} else if (consp(selected_imf->notes)) {
				if (stringp(car(selected_imf->notes))) {
					strcat(spbuf, " ");
					strcat(spbuf, c_string(car(selected_imf->notes)));
				} else {
					strcat(spbuf, " <notes>");
				}
			} else {
				strcat(spbuf, " <notes>");
			}
		}
		strcat(spbuf, "]");
		c2p(spbuf, tmpstr);
		MoveTo(4, 26);
		DrawString(tmpstr);
	}
	/* Draw a dividing line. */
	MoveTo(0, toplineh - 2);
	Line(imagewin->portRect.right, 0);
	/* Restore the existing port. */
	SetPort(oldport);
}

/* Draw a single imf at a given row and column. */

void
draw_one_image(imf, col, row)
ImageFamily *imf;
int col, row;
{
	int namex, namey;
	Rect tmprect, maskrect;
	FontInfo fontinfo;

	tmprect.left = col * eltw;  tmprect.top = toplineh + (row - firstvisrow) * elth;
	tmprect.right = tmprect.left + iw + 4;  tmprect.bottom = tmprect.top + ih + 4;
	InsetRect(&tmprect, 2, 2);
	/* Maybe draw the background terrain. (should be in a hex shape sometimes?) */
	if (vary_terrain) {
		draw_as_terrain_image(tmprect.left, tmprect.top, iw, ih, imf);
	} else if (with_terrain && const_terrain) {
		draw_as_terrain_image(tmprect.left-2, tmprect.top-2, iw+4, elth, const_terrain);
	}
	if (vary_unit) {
		draw_as_unit_image(imagewin, tmprect.left, tmprect.top, iw, ih, imf);
	} else if (with_unit && const_unit) {
		draw_as_unit_image(imagewin, tmprect.left, tmprect.top, iw, ih, const_unit);
	}
	/* Maybe draw an emblem. */
	if (vary_emblem) {
		draw_as_emblem_image(imagewin, tmprect.left + iw - 8, tmprect.top, 8, 8, imf);
	} else if (with_emblem && const_emblem) {
		draw_as_emblem_image(imagewin, tmprect.left + iw - 8, tmprect.top, 8, 8, const_emblem);
	}
	/* Maybe draw the name of the image. */
	if (show_names) {
		/* If nonwhite background, add a white rect for the name. */
		namex = col * eltw + iw + 4;
		namey = toplineh + (row - firstvisrow) * elth + (elth / 2) + 5;
		if (with_terrain && const_terrain) {
			GetFontInfo(&fontinfo);
			maskrect.left = namex;
			maskrect.top = namey - fontinfo.ascent;
			maskrect.right = maskrect.left + TextWidth(imf->name, 0, strlen(imf->name));
			maskrect.bottom = namey + fontinfo.descent + 1;
			FillRect(&maskrect, QD(white));
		}
		MoveTo(namex, namey);
		c2p(imf->name, tmpstr);
		DrawString(tmpstr);
	}
}

/* Adjust the scrollbar to reflect the size at which the images are being rendered. */

void
set_scrollbar()
{
	SetCtlMax(vscrollbar, rows - numvisrows);
	SetCtlValue(vscrollbar, firstvisrow);
	HiliteControl(vscrollbar, (numvisrows < rows ? 0 : 255));
}

/* Indicate which image is currently selected. */

void
invert_selected_imf()
{
	int row, col;
	Rect tmprect;

	if (selected_imf != NULL) {
		col = selected_n % cols;
		row = selected_n / cols;
		/* Calculate the bounding box for the selected image. */
		tmprect.left = col * eltw;  tmprect.top = toplineh + (row - firstvisrow) * elth;
		tmprect.right = tmprect.left + eltw;  tmprect.bottom = tmprect.top + elth;
		if (tmprect.top < toplineh) return;
		/* This inverts a rectangle around the selected image. */
		InvertRect(&tmprect);
		InsetRect(&tmprect, 1, 1);
		InvertRect(&tmprect);
	}
}

void
draw_as_terrain_image(int sx, int sy, int sw, int sh, ImageFamily *imf)
{
	Rect rect;
	Image *timg;
	MacImage *macimg;

	timg = best_image(imf, sw, sh);
	if (timg) {
		rect.left = sx;  rect.top = sy;
		rect.right = sx + sw;  rect.bottom = sy + sh;
		macimg = (MacImage *) timg->hook;
		if (macimg == NULL) {
			/* a serious error? */
		} else if (useColorQD && show_color && macimg->colrpat != nil) {
			FillCRect(&rect, macimg->colrpat);
		} else if (macimg->patdefined) {
			FillRect(&rect, (unsigned char *) &(macimg->monopat));
		} else {
			/* If no imagery, just leave blank, don't try to make a default image. */
		}
	}
}

/* This is similar (but not identical! beware!) to Xconq's main unit drawing routine,
   but it uses an arbitrary image family instead. */

void
draw_as_unit_image(WindowPtr win, int sx, int sy, int sw, int sh, ImageFamily *imf)
{
	Rect srcrect, imagerect;
	BitMap bm, *winbits;
	Image *uimg;
	MacImage *macimg;

	uimg = best_image(imf, sw, sh);
	if (uimg) {
		if ((uimg->istile ? !select_tiles : !select_icons))
		  return;
		imagerect = win->portRect;
		imagerect.top += sy;  imagerect.left += sx;
		imagerect.bottom = imagerect.top + sh;  imagerect.right = imagerect.left + sw;
		winbits = &(((GrafPtr) win)->portBits);
		macimg = (MacImage *) uimg->hook;
		if (macimg == NULL) {
			/* a serious error? */
		} else if (macimg->monopict != nil) {
			DrawPicture(macimg->monopict, &imagerect);
		} else if (useColorQD && show_color && macimg->colricon != nil) {
			PlotCIcon(&imagerect, (CIconHandle) macimg->colricon);
		} else if (macimg->monoicon != nil) {
			SetRect(&srcrect, 0, 0, 32, 32);
			bm.rowBytes = 4;
			bm.bounds = srcrect;
			if (macimg->maskicon != nil && show_mask) {
				bm.baseAddr = *(macimg->maskicon);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QD(white));
			}
			bm.baseAddr = *(macimg->monoicon);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else if (macimg->monosicn != nil) {
			SetRect(&srcrect, 0, 0, 16, 16);
			bm.rowBytes = 2;
			bm.bounds = srcrect;
			if (macimg->masksicn != nil && show_mask) {
				bm.baseAddr = *(macimg->masksicn);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QD(white));
			}
			bm.baseAddr = *(macimg->monosicn);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else if ((useColorQD && show_color && macimg->colrpat) || macimg->patdefined) {
			draw_as_terrain_image(sx, sy, sw, sh, imf);
		} else {
			/* should never be possible? */
		}
	}
}

/* Draw a given side's emblem. Uses the current GrafPort. */

void
draw_as_emblem_image(WindowPtr win, int ex, int ey, int ew, int eh, ImageFamily *imf)
{
	Rect srcrect, imagerect;
	BitMap bm, *winbits;
	Image *eimg;
	MacImage *macimg;

	eimg = best_image(imf, ew, eh);
	/* If an image is present, display it, otherwise just suppress. */
	if (eimg) {
		if ((eimg->istile ? !select_tiles : !select_icons))
		  return;
		imagerect = win->portRect;
		imagerect.top += ey;  imagerect.left += ex;
		imagerect.bottom = imagerect.top + eh;  imagerect.right = imagerect.left + ew;
		winbits = &(((GrafPtr) win)->portBits);
		macimg = (MacImage *) eimg->hook;
		if (macimg == NULL) {
			/* a serious error? */
		} else if (macimg->monopict != nil) {
			DrawPicture(macimg->monopict, &imagerect);
		} else if (useColorQD && show_color && macimg->colricon != nil) {
			PlotCIcon(&imagerect, (CIconHandle) macimg->colricon);
		} else if (macimg->monoicon != nil) {
			SetRect(&srcrect, 0, 0, 32, 32);
			bm.rowBytes = 4;
			bm.bounds = srcrect;
			if (macimg->maskicon != nil && show_mask) {
				bm.baseAddr = *(macimg->maskicon);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QD(white));
			}
			bm.baseAddr = *(macimg->monoicon);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else if (macimg->monosicn != nil) {
			SetRect(&srcrect, 0, 0, 16, 16);
			bm.rowBytes = 2;
			bm.bounds = srcrect;
			if (macimg->masksicn != nil && show_mask) {
				bm.baseAddr = *(macimg->masksicn);
				CopyBits(&bm, winbits, &srcrect, &imagerect, srcBic, nil);
			} else {
				/* Draw unit bbox as default mask. (maybe shrink a little??) */
				FillRect(&imagerect, QD(white));
			}
			bm.baseAddr = *(macimg->monosicn);
			CopyBits(&bm, winbits, &srcrect, &imagerect, srcOr, nil);
		} else {
			/* should never be possible? */
		}
	}
}

/* Lisp reader support. */

void
announce_read_progress()
{
}

char *
copy_pascal_string(char *str)
{
    int len = str[0];
    char *rslt;

    rslt = (char *) xmalloc(len + 1);
    strncpy(rslt, str+1, len);
    rslt[len] = '\0';
    return rslt;
}

void
low_init_warning(str)
char *str;
{
	/* Cursor may be weird from loading, reset it. */
	SetCursor(&QD(arrow));
	c2p(str, tmpstr);
	ParamText(tmpstr, "\p", "\p", "\p");
	switch (CautionAlert(aWarning, nil)) {
		case 1:
			/* Just keep going, hope that the warning was a false alarm. */
			/* (if option key on, should suppress future warnings) */
			break;
		default:
			ExitToShell();
			break;
	}
}

/* An error is immediately fatal, no recourse. */

void
low_init_error(str)
char *str;
{
	/* Cursor may be weird from loading, reset it. */
	SetCursor(&QD(arrow));
	c2p(str, tmpstr);
	ParamText(tmpstr, "\p", "\p", "\p");
	StopAlert(aError, nil);
	ExitToShell();
}

/* Map these to init_ versions, no "running" in this program. */

void
low_run_warning(str)
char *str;
{
	low_init_warning(str);
}

void
low_run_error(str)
char *str;
{
	low_init_error(str);
}

int
keyword_code(char *str)
{
    run_warning("fake keyword_code being called");
	return 0;
}

/* Make the table so keyword lookup works. */

struct a_key {
    char *name;
} keywordtable[] = {

#undef  DEF_KWD
#define DEF_KWD(NAME,code)  { NAME },

#include "keyword.def"

    { NULL }
};

char *
keyword_name(enum keywords k)
{
    return keywordtable[k].name;
}

/* Fake definitions of unneeded routines called by lisp.c. */

void
init_predefined_symbols()
{
}

int
lazy_bind(Obj *sym)
{
	return 0;
}

void
prealloc_debug()
{
}


