/* Main program of the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern int checkpointinterval;
extern int statistics_wanted;
extern int numsoundplays;
extern int numremotewaiting;
extern void low_send(int id, char *buf);
extern int low_receive(int *id, char *buf, int maxchars, int timeout);
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
#include "macconq.h"

#include <errors.h>
#include <time.h>

#ifdef __MWERKS__
#include <Sound.h>
#endif

#define keyReplyErr 'errn'

/* Function prototypes. */

static void handle_event(EventRecord *event);

static pascal OSErr do_ae_open_application(AppleEvent *message, AppleEvent *reply, long refcon);
static pascal OSErr do_ae_open_documents(AppleEvent *message, AppleEvent *reply, long refcon);
static pascal OSErr do_ae_print_documents(AppleEvent *message, AppleEvent *reply, long refcon);
static pascal OSErr do_ae_quit_application(AppleEvent *message, AppleEvent *reply, long refcon);
static pascal OSErr do_ae_join_game(AppleEvent *message, AppleEvent *reply, long refcon);

static Boolean missed_any_parameters(AppleEvent *message);

static pascal Boolean filter_warning_alert(DialogPtr dialog, EventRecord *evt, short *itemhit);

/* Global variables. */

/* This is the id of any map modal tool currently in effect. */

int map_modal = 0;

int inbackground;

/* This is the list of maps that we're using. */

struct a_map *maplist;

/* This is the list of lists. */

struct a_list *listlist;

/* This is the list of unit closeups. */

struct a_unit_closeup *unitcloseuplist;

/* This indicates whether the general game resource files was found. */

int foundresourcesfile = FALSE;

/* This flag indicates whether the image etc resource file(s) were found. */

int foundimagesfile = FALSE;

/* The usual width of a scrollbar. */

int sbarwid = 15;

/* True if new maps should show the cell grid. */

int default_draw_grid = TRUE;

/* True if new maps should display names and numbers. */

int default_draw_names = FALSE;

/* (should use these eventually instead) */

int default_draw_unit_names = FALSE;
int default_draw_unit_numbers = FALSE;
int default_draw_feature_names = FALSE;

/* True if we're going to use WaitNextEvent. */

int useWNE = FALSE;

/* Rectangle that constrains window dragging. */

Rect dragrect;

/* Rectangle that constrains window resizing. */

Rect sizerect;

/* This is the side that is using this Mac as its display. */

Side *dside = NULL;

/* This points to a spare block of memory that is freed so shutdown code can
   use it (no guarantee that it will tho). */

Handle spare;

/* This is true if savable prefs etc have been saved since being changed. */
/* (always true for now, nothing being remembered) */

int interfacestatesafe = TRUE;

/* This is true when a single click suffices to move a unit. */

int defaultmoveonclick = TRUE;

int defaultautoselect = TRUE;

int wasingame = TRUE;

int playsounds = TRUE;

/* Set to true if Color QuickDraw is installed. */

int hasColorQD;

/* The range of screen pixel depths that the display has to cope with. */

int minscreendepth = -1;

int maxscreendepth = -1;

/* This is true if AppleEvents are available. */

int hasAppleEvents;

/* This is true if the PPC toolbox is available. */

int hasPPCToolbox;

char *curdatestr = NULL;

int eventloopdone = FALSE;

int inputinvalid = FALSE;

/* True when a won or lost dialog has been put up on the screen already. */

static int told_outcome = FALSE;

MovieType movie_types[10];

struct a_movie {
  enum movie_type type;
  int args[5];
};

/* Number of movies waiting to be played. */

int numscheduled;

/* Movies to be played. */

struct a_movie movies[10];

int connection_method;

int hosting;

ModalFilterUPP filter_warning_alert_proc;

AEEventHandlerUPP do_ae_open_application_proc;
AEEventHandlerUPP do_ae_open_documents_proc;
AEEventHandlerUPP do_ae_print_documents_proc;
AEEventHandlerUPP do_ae_quit_application_proc;
AEEventHandlerUPP do_ae_join_game_proc;

/* The main Mac program. */

int
main()
{
	/* Do the most basic Macintosh setup. */
	init_toolbox();
	init_cursors();
	init_patterns();
	init_icons();
	init_menus();
	init_rects();
	/* Put the Xconq kernel into a known state. */
	clear_game_modules();
	init_data_structures();
	init_library_path(NULL);
	init_ae();
	/* Acquire Mac-specific files (preferences and resources). */
	get_files();
	/* A hack to ensure some memory available for error handling. */
	spare = NewHandle(2000);
	/* If no Apple Events, go to the splash screen now, otherwise we'll wait
	   for an oapp/odoc/pdoc event to decide what to do. */
	if (!hasAppleEvents) {
		if (splash_dialog() == diSplashQuit)
		  return 0;
	}
	/* All essential init done, jump into the main event loop. */
	event_loop();
	/* Will exit here, or perhaps via error. */
	return 0;
}

/* This is the first dialog that the user sees.  It doesn't do much
   besides provide the initial choice into the program proper. */

int
splash_dialog()
{
	switch (do_splash_box()) {
		case diSplashNew:
			new_game_dialog();
			break;
		case diSplashOpen:
			open_game_dialog();
			break;
		case diSplashConnect:
			hosting = FALSE;
			connect_game_dialog();
			break;
		case diSplashQuit:
			return diSplashQuit;
	}
	return -1;
}

/* Do the usual Mac setup calls. */

void
init_toolbox()
{
	SysEnvRec se;

	MaxApplZone();

	InitGraf(&QD(thePort));
	InitFonts();
	FlushEvents(everyEvent, 0);
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(NULL);
	InitCursor();

	/* We need to set this one up early, since may be used in error
	   reporting. */
    filter_warning_alert_proc = NewModalFilterProc(filter_warning_alert);

	SysEnvirons(2, &se);
	hasColorQD = se.hasColorQD;
	DGprintf("%s Color QuickDraw\n", (hasColorQD ? "Using" : "Not using"));
	recalc_depths();
}

/* Look at all the devices and compute the range of screen depths. */

void
recalc_depths()
{
	int depth, oldmin = minscreendepth, oldmax = maxscreendepth;
	GDHandle gdev;

	if (hasColorQD) {
		gdev = GetDeviceList();
		minscreendepth = maxscreendepth = (*((*gdev)->gdPMap))->pixelSize;
		while ((gdev = GetNextDevice(gdev)) != nil) {
			depth = (*((*gdev)->gdPMap))->pixelSize;
			if (depth < minscreendepth)
			  minscreendepth = depth;
			if (depth > maxscreendepth)
			  maxscreendepth = depth;
		}
	} else {
		minscreendepth = maxscreendepth = 1;
	}
	if (minscreendepth != oldmin || maxscreendepth != oldmax) {
		DGprintf("Screen depths range from %d to %d\n", minscreendepth, maxscreendepth);
	}
}

/* Set up the generic dragging and sizing rects. */

void
init_rects()
{
	RgnHandle screenrgn;

	screenrgn = GetGrayRgn();
	dragrect = (*screenrgn)->rgnBBox;
	SetRect(&sizerect, 50, 50, (*screenrgn)->rgnBBox.right,  (*screenrgn)->rgnBBox.bottom);
}

/* Basic Apple Event handling. */

void
init_ae()
{
	OSErr err;
	long rslt;

	hasPPCToolbox  = (Gestalt(gestaltPPCToolboxAttr, &rslt) ? false : (rslt != 0));
	hasAppleEvents = (Gestalt(gestaltAppleEventsAttr, &rslt) ? false : (rslt != 0));

	if (hasAppleEvents) {
		do_ae_open_application_proc = NewAEEventHandlerProc(do_ae_open_application);
		do_ae_open_documents_proc = NewAEEventHandlerProc(do_ae_open_documents);
		do_ae_print_documents_proc = NewAEEventHandlerProc(do_ae_print_documents);
		do_ae_quit_application_proc = NewAEEventHandlerProc(do_ae_quit_application);
		do_ae_join_game_proc = NewAEEventHandlerProc(do_ae_join_game);

		err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication,
									do_ae_open_application_proc, 0L, false);
		if (err) {
			init_warning("AppleEvent handler could not be installed, skipping others");
			return;
		}
		err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments,
									do_ae_open_documents_proc, 0L, false);
		if (err) {
			init_warning("AppleEvent handler could not be installed, skipping others");
			return;
		}
		err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments,
									do_ae_print_documents_proc, 0L, false);
		if (err) {
			init_warning("AppleEvent handler could not be installed, skipping others");
			return;
		}
		err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication,
									do_ae_quit_application_proc, 0L, false);
		if (err) {
			init_warning("AppleEvent handler could not be installed, skipping others");
			return;
		}
		err = AEInstallEventHandler('xcnq', 'join',
									do_ae_join_game_proc, 0L, false);
		if (err) {
			init_warning("AppleEvent handler could not be installed, skipping others");
			return;
		}
	}
}

short prefs_refnum = -1;

/* Open and/or load any files that we might need, such as preferences
   and resources. */

void
get_files()
{
	Str255 filename;
	extern short initialvrefnum;

	/* Capture the current vrefnum. */
	GetVol(NULL, &initialvrefnum);
	/* Load up any preferences. */
	/* As a concession to Mac-ness, we disable statistics writing by default, and require
	   a preference that it be on.  This can be removed when the user gets to choose the
	   name of the statistics file before it is written. */
	statistics_wanted = FALSE;
	get_preferences();
	/* Look for and open game library resource files. */
	foundresourcesfile = FALSE;
	GetIndString(filename, sFilenames, siResources);
	if (OpenResFile(filename) != -1) {
		foundresourcesfile = TRUE;
	}
	foundimagesfile = FALSE;
	GetIndString(filename, sFilenames, siImages);
	if (OpenResFile(filename) != -1) {
		foundimagesfile = TRUE;
	}
	GetIndString(filename, sFilenames, siSounds);
	if (OpenResFile(filename) != -1) {
		/* (need to do anything special if sounds not found??) */
	}
	/* Note that we don't complain yet if the resource/image files are missing,
	   since we don't yet know whether we actually need anything from them.
	   (Images etc might be built into app or game module, for instance.) */
}

int
open_preferences()
{
	short vref;
	long dirid;
	OSErr err;
	Str255 filename;

	GetIndString(filename, sFilenames, siPreferences);
	/* (should do a gestalt check first) */
	err = FindFolder(kOnSystemDisk, kPreferencesFolderType, kDontCreateFolder,
				   &vref, &dirid);
	if (err != noErr) {
		SysEnvRec se;

		err = SysEnvirons(1, &se);
		if (err == noErr) {
			vref = se.sysVRefNum;
		} else {
			vref = 0;
		}
		dirid = 0;
	}
	err = HCreate(vref, dirid, filename, XconqSignature, 'pref');
	if (err == dupFNErr)
	  err = noErr;
	if (err == noErr) {
		/* Ensure that the resource fork exists. */
		HCreateResFile(vref, dirid, filename);
		prefs_refnum = HOpenResFile(vref, dirid, filename, fsRdWrPerm);
		if (prefs_refnum == -1)
			err = -1;
		if (ResError())
			err = ResError();
	}
	return err;
}

void
close_preferences()
{
	CloseResFile(prefs_refnum);
	prefs_refnum = -1;
}

void
get_preferences()
{
	int startlineno = 0, endlineno = 0;
	Obj *uispec;
	OSErr err;
	Handle prefs;

	err = open_preferences();
	if (err == noErr) {
		UseResFile(prefs_refnum);
		prefs = GetResource('XCpf', 128);
		if (prefs != nil) {
			uispec = read_form_from_string(copy_string(*prefs), &startlineno, &endlineno);
			interp_mac_ui_data(uispec);
		}
		close_preferences();
	}
}

void
save_preferences()
{
	int cur_refnum;
	char prefbuf[300];
	OSErr err;
	Handle oldprefs, prefs;

	err = open_preferences();
	if (err == noErr) {
		ui_save_state(dside);
		sprintlisp(prefbuf, find_at_key(dside->uidata, "mac"));
		/* Copy the string into a handle that will be used for the resource. */
		prefs = NewHandle(strlen(prefbuf) + 1);
		HLock(prefs);
		strcpy(*prefs, prefbuf);
		HUnlock(prefs);
		cur_refnum = CurResFile();
		UseResFile(prefs_refnum);
		oldprefs = GetResource('XCpf', 128);
		if (oldprefs != nil) {
			RmveResource(oldprefs);
			UpdateResFile(prefs_refnum);
		}
		AddResource(prefs, 'XCpf', 128, "\pXconq Preferences");
		close_preferences();
		UseResFile(cur_refnum);
	}
}

void
interp_mac_ui_data(Obj *uispec)
{
	int numval;
	char *name;
	Obj *rest, *bdg;

	for (rest = uispec; rest != lispnil; rest = cdr(rest)) {
		bdg = car(rest);
		if (!consp(bdg)) {
			/* Don't complain out loud normally, confusing to users
			   because preferences are under Xconq and not user control. */
			Dprintf("Syntax error in preference binding?\n");
			continue;
		}
		if (symbolp(car(bdg))) {
			name = c_string(car(bdg));
			numval = 0;
			if (numberp(cadr(bdg))) {
				numval = c_number(cadr(bdg));
			} else {
				Dprintf("Preference property `%s' has non-numeric value, setting to zero\n",
						name);
			}
			if (strcmp(name, "sound") == 0) {
				playsounds = numval;
			} else if (strcmp(name, "default-draw-grid") == 0) {
				default_draw_grid = numval;
			} else if (strcmp(name, "default-draw-names") == 0) {
				default_draw_names = numval;
			} else if (strcmp(name, "checkpoint-interval") == 0) {
				checkpointinterval = numval;
			} else if (strcmp(name, "want-statistics") == 0) {
				statistics_wanted = numval;
			} else {
				/* Ignore unrecognized properties. */
				Dprintf("Preference binding `%s' unrecognized\n", name);
			}
		} else {
			/* As with above comment. */
			Dprintf("Syntax error in preference binding head?\n");
		}
	}
}

void
ui_save_state(Side *side)
{
	/* We don't do anything here because the ui data is kept up-to-date. */
}

void
ui_update_state()
{
	Obj *state = lispnil;

	push_binding(&state, intern_symbol("want-statistics"), new_number(statistics_wanted));
	push_binding(&state, intern_symbol("checkpoint-interval"), new_number(checkpointinterval));
	push_binding(&state, intern_symbol("default-draw-grid"), new_number(default_draw_grid));
	push_binding(&state, intern_symbol("default-draw-names"), new_number(default_draw_names));
	push_binding(&state, intern_symbol("sound"), new_number(playsounds));
	dside->uidata = replace_at_key(dside->uidata, "mac", state);
}

/* Dialogue to set the preferences. */

void
set_preferences()
{
	short done = FALSE, ditem;
	DialogPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	win = GetNewDialog(dPreferences, NULL, (DialogPtr) -1L);
	/* Set the current preferences into the items. */
	GetDItem(win, diPrefsGrid, &itemtype, &itemhandle, &itemrect);
	SetCtlValue((ControlHandle) itemhandle, default_draw_grid);
	GetDItem(win, diPrefsNames, &itemtype, &itemhandle, &itemrect);
	SetCtlValue((ControlHandle) itemhandle, default_draw_names);
	GetDItem(win, diPrefsCheckpoint, &itemtype, &itemhandle, &itemrect);
	SetCtlValue((ControlHandle) itemhandle, (checkpointinterval > 0));
	GetDItem(win, diPrefsStatistics, &itemtype, &itemhandle, &itemrect);
	SetCtlValue((ControlHandle) itemhandle, statistics_wanted);
	ShowWindow(win);
	while (!done) {
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diPrefsOK:
				/* Actually change the program's variables. */
				GetDItem(win, diPrefsGrid, &itemtype, &itemhandle, &itemrect);
				default_draw_grid = GetCtlValue((ControlHandle) itemhandle);
				GetDItem(win, diPrefsNames, &itemtype, &itemhandle, &itemrect);
				default_draw_names = GetCtlValue((ControlHandle) itemhandle);
				GetDItem(win, diPrefsCheckpoint, &itemtype, &itemhandle, &itemrect);
				checkpointinterval = (GetCtlValue((ControlHandle) itemhandle) ? 5 : 0);
				GetDItem(win, diPrefsStatistics, &itemtype, &itemhandle, &itemrect);
				statistics_wanted = GetCtlValue((ControlHandle) itemhandle);
				/* Remember all these settings. */
				ui_update_state();
				save_preferences();
				/* Fall into next case. */
			case diPrefsCancel:
				done = TRUE;
				break;
			case diPrefsGrid:
			case diPrefsNames:
			case diPrefsStatistics:
				/* Toggle check boxes. */
				GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
				SetCtlValue((ControlHandle) itemhandle, !GetCtlValue((ControlHandle) itemhandle));
				break;
			case diPrefsCheckpoint:
				GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
				SetCtlValue((ControlHandle) itemhandle, !GetCtlValue((ControlHandle) itemhandle));
				/* Should enable/disable interval-setting ditems. */
				break;
			case diPrefsInterval:
				break;
		}
	}
	DisposDialog(win);
}

/* Since Mac programs effectively take over the entire machine, we depend on
   this event loop to handle everything that might come along.  */

void
event_loop()
{
	int done = FALSE;
	Boolean gotevent;
	Point mouse;
	EventRecord	event;
	RgnHandle cursorRgn;
	short itemhit;
	DialogPtr dialog;

	/* Figure out if the WaitNextEvent Trap is available. */
	useWNE = (NGetTrapAddress(0x60, ToolTrap) != NGetTrapAddress(0x9f, ToolTrap));
	/* Pass WNE an empty region the 1st time thru. */
	cursorRgn = NewRgn();
	/* Loop (almost) forever. */
	while (!eventloopdone) {
		/* Use WaitNextEvent if it is available, otherwise GetNextEvent. */
		if (useWNE) {
			get_global_mouse(&mouse);
			adjust_cursor(mouse, cursorRgn);
			gotevent = WaitNextEvent(everyEvent, &event, 0L, cursorRgn);
		} else {
			SystemTask();
			gotevent = GetNextEvent(everyEvent, &event);
		}
		/* First decide if the event is for a dialog or is just any old event. */
		if (FrontWindow() != nil && IsDialogEvent(&event)) {
			/* Handle all the modeless dialogs here. */
			/* Process any player setup window events that we want to handle
			   specially (such as keystrokes). */
			if (playersetupwin != nil && playersetupwin == FrontWindow())
			  handle_player_setup_event(&event);
			if (DialogSelect(&event, &dialog, &itemhit)) {
				if (dialog == instructionswin) {
					hit_instructions_dialog(dialog, itemhit, &event);
				}
				/* Process player setup window events that the Dialog
				   Manager handles (clicks on buttons for instance). */
				if (dialog == playersetupwin) {
					mouse = event.where;
					SetPort(playersetupwin);
					GlobalToLocal(&mouse);
					if (hit_player_setup_dialog(itemhit, mouse))
					  launch_game_2();
				}
				/* If this was something like a key or click event, but wasn't handled
				   by the dialog, we're done and can go wait for the next event.
				   Otherwise, for null events, fall through to the usual handling. */
				if (gotevent)
				  continue;
			}
		}
		if (gotevent) {
			/* Make sure we have the right cursor before handling the event. */
			adjust_cursor(event.where, cursorRgn);
			handle_event(&event);
		} else if (beforestart && hosting && connection_method > 0 && numremotewaiting > 0) {
			int gotsome, remoteid;

			gotsome = low_receive(&remoteid, NULL, 0, 0);
			if (gotsome) {
				add_remote_player(NULL);
			}
		} else if (!beforestart && !endofgame) {
			/* On null events, give the kernel a chance to run something. */
			/* DON'T automatically go to a watch cursor, since run_game often
			   returns very quickly.  Instead, long-running subroutines should
			   call back to put a watch cursor up. */
			run_game(1);
			maybe_select_next_unit();
			/* If the game ended, force various changes in interaction. */
			if (endofgame)
			  set_end_of_game_interaction_modes();
		}
		/* Blink TE cursors regularly. */
		if (commandwin != nil && FrontWindow() == commandwin) {
			TEIdle(command_text);
		}
	}
}

void
get_global_mouse(Point *mouse)
{
	EventRecord	evt;
	
	OSEventAvail(0, &evt);
	*mouse = evt.where;
}

void
set_end_of_game_interaction_modes()
{
	Map *map;

	for_all_maps(map) {
		map->moveonclick = map->autoselect = FALSE;
		force_update(map->window);
	}
}

Point lastmouse;

char mouseoverbuf[100];

/* Change the cursor to reflect the context. */

void
adjust_cursor(Point mouse, RgnHandle region)
{
	int x, y, approxdir = 1, usual = TRUE;
	Unit *unit = NULL;
	extern char *mouseover;
	Map *map;
	CursPtr adjust_designer_cursor();
	GrafPtr oldport;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		GetPort(&oldport);
		SetPort(map->window);
		GlobalToLocal(&mouse);
		if (mouse.h > conwid && mouse.h < map->window->portRect.right - sbarwid
			&& mouse.v > map->toph && mouse.v < map->window->portRect.bottom - sbarwid) {
			if (map_modal != NO_MODAL) {
				switch (map_modal) {
					case FIRE_MODAL:
					case FIRE_INTO_MODAL:
						SetCursor(*firecursor);
						break;
					case MOVE_TO_MODAL:
					case SET_FORMATION_MODAL:
					case ADD_TERRAIN_MODAL:
					case REMOVE_TERRAIN_MODAL:
					case DISTANCE_MODAL:
					case ZOOM_MODAL:
					case GENERIC_MODAL:
						SetCursor(*opencrosscursor);
						break;
					default:
						run_error("unknown modal tool %d", map_modal);
						break;
				}
				usual = FALSE;
#ifdef DESIGNERS
			} else if (dside->designer && tooltype != notool) {
				SetCursor(adjust_designer_cursor(mouse, region));  usual = FALSE;
#endif DESIGNERS
			} else if (map->moveonclick) {
				if (map->numselections == 1
				    && (unit = map->selections[0]) != NULL) {
					/* Calculate the approx dir to here from selected unit. */
					m_nearest_cell(map, mouse.h, mouse.v, &x, &y);
					/* Note that we allow x,y here to be outside the world. */
					if (mobile(unit->type)
					    && (approxdir = approx_dir(x - unit->x, y - unit->y)) >= 0) {
						SetCursor(*(movecursors[approxdir]));  usual = FALSE;
					} else {
						SetCursor(*nomovecursor);  usual = FALSE;
					}
				} else if (map->numselections > 1) {
					SetCursor(*allmovecursor);  usual = FALSE;
				} else {
					/* (this is a little confusing here if no units are selected, since
				   	will just be arrow cursor) */
				}
			}
			/* This isn't really "cursor adjustment", but this is the right place
			   to do it - change the topline of the map to indicate what the cursor
			   is over. */
			if (map->toplineh > 0 && !EqualPt(mouse, lastmouse)) {
				oneliner(map, mouse.h, mouse.v);
				if (strcmp(tmpbuf, mouseoverbuf) != 0) {
					strcpy(mouseoverbuf, tmpbuf);
					mouseover = mouseoverbuf;
					draw_top_line(map);
				}
				lastmouse = mouse;
			}
		} else {
			if (map->toplineh > 0) {
				if (mouseover != NULL) {
					mouseover = NULL;
					draw_top_line(map);
				}
			}
		}
		SetPort(oldport);
	}
	
	if (endofgame || (!beforestart && dside && !dside->ingame)) {
		SetCursor(*grayarrowcursor);
		return;
	}
	/* If we got here and no cursor has been set already, go with the basic arrow. */
	if (usual)
	  SetCursor(&QD(arrow));
}

/* Decipher an event. */

void
handle_event(EventRecord *event)
{
	short part, err;
	WindowPtr win;
	char key;
	Point pnt;

	switch (event->what) {
		case mouseDown:
			/* See if the click happened in a special part of the screen. */
			part = FindWindow(event->where, &win);
			switch (part) {
				case inMenuBar:
					adjust_menus();
					do_menu_command(MenuSelect(event->where));
					break;
				case inSysWindow:
					SystemClick(event, win);
					break;
				case inContent:
					if (win != FrontWindow()) {
						/* Bring the clicked-on window to the front. */
						SelectWindow(win);
						/* Fix the menu to match the new front window. */
						adjust_menus();
						/* We always want to discard the event now, since clicks in a
						   windows are often irreversible actions. */
					} else {
						/* Mouse clicks in the front window do something useful. */
						do_mouse_down(win, event);
					}
					break;
				case inDrag:
					/* Standard drag behavior, no tricks necessary. */
					DragWindow(win, event->where, &dragrect);
					break;
				case inGrow:
					grow_window(win, event->where);
					break;
				case inZoomIn:
				case inZoomOut:
					zoom_window(win, event->where, part);
					break;
				case inGoAway:
					if (TrackGoAway(win, event->where))
					  close_window(win);
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
					int handled = FALSE;

					/* Random keypress, interpret it. */
					if (FrontWindow() == constructionwin)
					  handled = do_key_down_construction(key);
					if (FrontWindow() == commandwin)
					  handled = do_key_down_command(key);
					if (!handled)
					  do_keyboard_command(key);
				}
			}
			break;
		case activateEvt:
			activate_window((WindowPtr) event->message, event->modifiers & activeFlag);
			break;
		case updateEvt:
			update_window((WindowPtr) event->message);
			break;
		case diskEvt:
			/*	Call DIBadMount in response to a diskEvt, so that the user can format
				 a floppy. (from DTS Sample) */
			if (HiWord(event->message) != noErr) {
				SetPt(&pnt, 50, 50);
				err = DIBadMount(pnt, event->message);
			}
			break;
		case app4Evt:
		    /* Grab only a single byte. */
			switch ((event->message >> 24) & 0xFF) {
				case 0xfa:
					break;
				case 1:
					inbackground = !(event->message & 1);
					activate_window(FrontWindow(), !inbackground);
					break;
			}
			break;
		case kHighLevelEvent:
			AEProcessAppleEvent(event);
			break;
#if 0 /* null events don't come through here */
		case nullEvent:
			break;
#endif
			break;
		default:
			break;
	}
#ifdef DEBUGGING
	/* This just forces output into the file. */
	update_debugging();
#endif
}

/* Handle window growing by mindlessly tracking via GrowWindow,
   then passing the chosen size to specific window resize handlers
   or else doing the generic resize. */

void
grow_window(WindowPtr win, Point where)
{
	long winsize;
	short winh, winv;
	GrafPtr oldport;

	if ((winsize = GrowWindow(win, where, &sizerect)) != 0) {
		GetPort(&oldport);
		SetPort(win);
		winh = LoWord(winsize);  winv = HiWord(winsize);
		if (map_from_window(win)) {
			grow_map(map_from_window(win), winh, winv);
		} else if (list_from_window(win)) {
			grow_list(list_from_window(win), winh, winv);
		} else if (win == historywin) {
			grow_history(winh, winv);
		} else if (win == constructionwin) {
			grow_construction(winh, winv);
		} else if (win == helpwin) {
			grow_help(winh, winv);
		} else if (win == noticewin) {
			grow_notice(winh, winv);
		} else if (win == scoreswin) {
			grow_scores(winh, winv);
		}
		SetPort(oldport);
	}
}

void
zoom_window(WindowPtr win, Point where, int part)
{
	GrafPtr oldport;

	if (TrackBox(win, where, part)) {
		GetPort(&oldport);
		/* The window must be the current port. (ZoomWindow bug) */
		SetPort(win);
		if (map_from_window(win)) {
			zoom_map(map_from_window(win), part);
		} else if (list_from_window(win)) {
			zoom_list(list_from_window(win), part);
		} else if (win == constructionwin) {
			zoom_construction(part);
		} else if (win == historywin) {
			zoom_history(part);
		} else if (win == helpwin) {
			zoom_help(part);
		} else if (win == noticewin) {
			zoom_notice(part);
		} else if (win == scoreswin) {
			zoom_scores(part);
		} else {
			/* Generic window zooming. */
			EraseRect(&win->portRect);
			ZoomWindow(win, part, true);
			InvalRect(&win->portRect);
		}
		SetPort(oldport);
	}
}

void
close_window(WindowPtr win)
{
	if (is_da_window(win)) {
		CloseDeskAcc(((WindowPeek) win)->windowKind);
	} else if (is_app_window(win)) {
		/* Remove from the windows menu (OK to call even if window not in menu). */
		remove_window_menu_item(win);
		/* Do special activities for some window subtypes. */
		if (map_from_window(win)) {
			destroy_map(map_from_window(win));
		} else if (list_from_window(win)) {
			destroy_list(list_from_window(win));
		} else if (unit_closeup_from_window(win)) {
			destroy_unit_closeup(unit_closeup_from_window(win));
#ifdef DESIGNERS
		} else if (win == designwin) {
			/* Closing the design palette implies we're done designing. */
			disable_designing();
#endif /* DESIGNERS */
		}
		/* Remove the window from our sight, will provoke update events. */
		HideWindow(win);
		/* At least for now, don't actually dispose of the window. */
	}
}

/* This just dispatches to the appropriate window handler. */

void
do_mouse_down(WindowPtr window, EventRecord *event)
{
	Point mouse;
	Map *map;
	List *list;
	UnitCloseup *unitcloseup;

	if (is_app_window(window)) {
		SetPort(window);
		mouse = event->where;
		GlobalToLocal(&mouse);
		/* Locate the interface object that this is on. */
		if ((map = map_from_window(window)) != NULL) {
			do_mouse_down_map(map, mouse, event->modifiers); 
		} else if ((list = list_from_window(window)) != NULL) {
			do_mouse_down_list(list, mouse, event->modifiers); 
		} else if ((unitcloseup = unit_closeup_from_window(window)) != NULL) {
			do_mouse_down_unit_closeup(unitcloseup, mouse, event->modifiers); 
		} else if (window == gamewin) {
			do_mouse_down_game(mouse, event->modifiers);
		} else if (window == historywin) {
			do_mouse_down_history(mouse, event->modifiers);
		} else if (window == constructionwin) {
			do_mouse_down_construction(mouse, event->modifiers);
		} else if (window == helpwin) {
			do_mouse_down_help(mouse, event->modifiers);
		} else if (window == noticewin) {
			do_mouse_down_notice(mouse, event->modifiers);
		} else if (window == commandwin) {
			do_mouse_down_command(mouse, event->modifiers);
		} else if (window == scoreswin) {
			do_mouse_down_scores(mouse, event->modifiers);
#ifdef DESIGNERS
		} else if (window == designwin) {
			do_mouse_down_design(mouse, event->modifiers);
#endif /* DESIGNERS */
		}
	} else {
		/* ??? */
	}
}

/* Bringing a window to the front may entail messing with the menu. */

void
activate_window(WindowPtr win, int activate)
{
	Map *map;
	List *list;

	if (win == nil)
	  return;
	if (activate) {
		/* It's convenient to make the activated window also be the current GrafPort. */
		SetPort(win);
	}
	adjust_menus();
	if ((map = map_from_window(win)) != NULL) {
		activate_map(map, activate);
	} else if ((list = list_from_window(win)) != NULL) {
		activate_list(list, activate);
	} else if (win == constructionwin) {
		activate_construction(activate);
	} else if (win == helpwin) {
		activate_help(activate);
	} else if (win == noticewin) {
		activate_notice(activate);
	} else if (win == commandwin) {
		activate_command(activate);
	} else if (win == scoreswin) {
		activate_scores(activate);
	} else {
		DGprintf("%sactivating random window\n", (activate ? "" : "de"));
	}
}

/* Update a given window.  This is the main routine that causes drawing into
   all the different kinds of windows. */

void
update_window(WindowPtr win)
{
	int controls = TRUE, growbox = FALSE;
	GrafPtr oldport;
	Map *map;
	List *list;
	UnitCloseup *unitcloseup;

	/* Set the updating window to be the current grafport. */
	GetPort(&oldport);
	SetPort(win);
	recalc_depths();
	BeginUpdate(win);
	if ((map = map_from_window(win)) != NULL) {
		draw_map(map);
		growbox = TRUE;
	} else if ((list = list_from_window(win)) != NULL) {
		draw_list(list);
		growbox = TRUE;
	} else if ((unitcloseup = unit_closeup_from_window(win)) != NULL) {
		draw_unit_closeup(unitcloseup);
	} else if (win == gamewin) {
		draw_game();
		controls = FALSE;
	} else if (win == historywin) {
		draw_history();
		growbox = TRUE;
	} else if (win == constructionwin) {
		draw_construction();
		growbox = TRUE;
	} else if (win == helpwin) {
		draw_help();
		growbox = TRUE;
	} else if (win == noticewin) {
		draw_notice();
		growbox = TRUE;
	} else if (win == commandwin) {
		draw_command();
		growbox = FALSE;
	} else if (win == scoreswin) {
		draw_scores();
		growbox = TRUE;
#ifdef DESIGNERS
	} else if (win == designwin) {
		draw_design_window();
		controls = FALSE;
#endif /* DESIGNERS */
	} else {
		controls = FALSE;
	}
	if (controls) {
		UpdateControls(win, win->visRgn);
	}
	if (growbox) {
		DrawGrowIcon(win);
	}
	EndUpdate(win);
	SetPort(oldport);
}

static int last_tick_count = 0;

void
maybe_select_next_unit()
{
	Unit *unit;
	Map *map;

	if ((!beforestart && !endofgame)
	    && (map = map_from_window(FrontWindow())) != NULL
	    && map->autoselect) {
	    /* Hunt around for a reasonable "next unit" to select. */
	    /* Scroll over to the current unit if appropriate. */
	    if (map->curunit != NULL
	    	&& in_play(map->curunit)
			&& (map->curunit->act
				&& map->curunit->act->acp > 0)  /* should be "above min"? */
	    	&& (map->curunit->plan
				&& !map->curunit->plan->asleep
				&& !map->curunit->plan->reserve
				&& !map->curunit->plan->delayed)
			&& map->scrolltocurunit
	    	) {
	    	scroll_to_unit(map, map->curunit);
	    	map->scrolltocurunit = FALSE;
	    	goto blink;
	    }
		unit = autonext_unit_inbox(dside, map->curunit, map->vp);
		if (unit
			&& unit->plan
			&& !unit->plan->asleep
			&& !unit->plan->reserve
			&& !unit->plan->delayed
			&& unit->plan->waitingfortasks
			) {
			map->curunit = unit;
			select_exactly_one_unit(map, map->curunit);
			goto blink;
		}
		/* Look for the next unit. */
		unit = find_next_awake_mover(dside, map->curunit);
		if (unit
			&& unit->plan
			&& !unit->plan->asleep
			&& !unit->plan->reserve
			&& !unit->plan->delayed
			&& unit->plan->waitingfortasks
			) {
			map->curunit = unit;
			select_exactly_one_unit(map, map->curunit);
			goto blink;
		}
		/* Start over from beginning of list. */
		unit = find_next_awake_mover(dside, NULL);
		if (unit
			&& unit->plan
			&& !unit->plan->asleep
			&& !unit->plan->reserve
			&& !unit->plan->delayed
			&& unit->plan->waitingfortasks
			) {
			map->curunit = unit;
			select_exactly_one_unit(map, map->curunit);
			goto blink;
		}
	  blink:
	    if (map->curunit != NULL) {
	        int tick_count;

			tick_count = TickCount();
	    	if (tick_count - last_tick_count >= 10) {
	    		last_tick_count = tick_count;
	    	    animation_pattern_state = (animation_pattern_state + 1) % 8;
	    	    draw_selection_animation(map, map->curunit);
	    	}
	    }
	}
}

/* Used to check for any unread required parameters. Returns true if we
   missed at least one. */

Boolean
missed_any_parameters(AppleEvent *message)
{
	OSErr err;
	DescType ignoredActualType;
	AEKeyword missedKeyword;
	Size ignoredActualSize;
	EventRecord	event;

	err = AEGetAttributePtr(message, keyMissedKeywordAttr, typeKeyword, &ignoredActualType,
						    (Ptr) &missedKeyword, sizeof(missedKeyword), &ignoredActualSize);
	/* No error means that we found some unused parameters. */
	if (err == noErr) {
		event.message = *(long *) &ignoredActualType;
		event.where = *(Point *) &missedKeyword;
		err = errAEEventNotHandled;
	}
	/* errAEDescNotFound means that there are no more parameters.  If we get
	   an error code other than that, flag it. */
	return (err != errAEDescNotFound);
}

static pascal OSErr
do_ae_open_application(AppleEvent *message, AppleEvent *reply, long refcon)
{
#pragma unused (message, refcon)
	OSErr err;

	if (splash_dialog() == diSplashQuit) {
		/* Set the global that lets the whole program exit. */
		eventloopdone = TRUE;
	}
	AEPutParamPtr(reply, keyReplyErr, typeShortInteger, (Ptr) &err, sizeof(short));
	return err;
}

/* Called when we receive an AppleEvent with an ID of "kAEOpenDocuments".
   This routine gets the direct parameter, parses it up into little FSSpecs,
   and opens the first indicated file.  It also shows the technique to be used in
   determining if you are doing everything the AppleEvent record is telling
   you.  Parameters can be divided up into two groups: required and optional.
   Before executing an event, you must make sure that you've read all the
   required events.  This is done by making an "any more?" call to the
   AppleEvent manager. */

static pascal OSErr
do_ae_open_documents(AppleEvent *message, AppleEvent *reply, long refcon)
{
#pragma unused (refcon)

	OSErr err, err2;
	AEDesc theDesc;
	FSSpec fsspec;
	short loop;
	long numFilesToOpen;
	AEKeyword ignoredKeyWord;
	DescType ignoredType;
	Size ignoredSize;

	theDesc.dataHandle = nil;

	err = AEGetParamDesc(message, keyDirectObject, typeAEList, &theDesc);
	if (err)
	  return err;
	if (!missed_any_parameters(message)) {
		/* Got all the parameters we need.  Now, go through the direct object,
		   see what type it is, and parse it up. */
		err = AECountItems(&theDesc, &numFilesToOpen);
		if (!err) {
			/* We have numFilesToOpen that need opening, as either a window
			   or to be printed.  Go to it... */
			for (loop = 1; ((loop <= numFilesToOpen) && (!err)); ++loop) {
				err = AEGetNthPtr(&theDesc, loop, typeFSS, &ignoredKeyWord, &ignoredType,
								  (Ptr) &fsspec, sizeof(fsspec), &ignoredSize);
				if (err)
				  break;
				if (open_game_from_fsspec(&fsspec))
				  break;
			}
		}
	}
	err2 = AEDisposeDesc(&theDesc);
	err = (err ? err : err2);
	AEPutParamPtr(reply, keyReplyErr, typeShortInteger, (Ptr) &err, sizeof(short));
	return err;
}

static pascal OSErr
do_ae_print_documents(AppleEvent *message, AppleEvent *reply, long refcon)
{
	OSErr err;

	AEPutParamPtr(reply, keyReplyErr, typeShortInteger, (Ptr) &err, sizeof(short));
	return err;
}

static pascal OSErr
do_ae_quit_application(AppleEvent *message, AppleEvent *reply, long refcon)
{
	OSErr err = noErr;

	/* Set the global that lets the whole program exit. */
	/* (should end the game reasonably first?) */
	eventloopdone = TRUE;
	AEPutParamPtr(reply, keyReplyErr, typeShortInteger, (Ptr) &err, sizeof(short));
	return noErr;
}

static pascal OSErr
do_ae_join_game(AppleEvent *message, AppleEvent *reply, long refcon)
{
#pragma unused (message, refcon)
	OSErr err = noErr;

	if (playersetupwin != nil) {
		add_remote_player(NULL);
	} else {
		beep();
		beep();
		beep();
	}
	return err;
}

void
connect_game_dialog()
{
	hosting = FALSE;
	connection_method_dialog();
	init_connection_method();
	if (connection_method > 0) {
		low_send(0, "join");
	}
}

void
connection_method_dialog()
{
	int done = FALSE, newmethod;
	short ditem;
	WindowPtr win;
	PicHandle pic;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	newmethod = 9;
	win = GetNewDialog(170, NULL, (DialogPtr) -1L);
	ShowWindow(win);
	SelectWindow(win);
	while (!done) {
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case 1:
				connection_method = newmethod;
				done = TRUE;
				break;
			case 2:
				done = TRUE;
				break;
			case 3:
			case 4:
			case 5:
				break;
			default:
				break;
		}
	}
	DisposDialog(win);
}

void init_serial_port(void);

void
init_connection_method()
{
	switch (connection_method) {
		case 0:
			break;
		case 1:
			init_serial_port();
			break;
		case 2:
			break;
		default:
			break;
	}
}

int ser_input_refnum;
int ser_output_refnum;

void
init_serial_port()
{
    OSErr err;

	if (1 /* using modem port */) {
		/* Open the modem port. */
		err = OpenDriver("\p.AOut", (short *) &ser_input_refnum);
		if (err != 0) {
			return;
		}
		err = OpenDriver("\p.AIn", (short *) &ser_output_refnum);
		if (err != 0) {
			CloseDriver(ser_input_refnum);
			return;
		}
	} else {
		/* Open the printer port. */
		err = OpenDriver("\p.BOut", (short *) &ser_input_refnum);
		if (err != 0) {
			return;
		}
		err = OpenDriver("\p.BIn", (short *) &ser_output_refnum);
		if (err != 0) {
			CloseDriver(ser_input_refnum);
			return;
		}
	}
}

#if 0
	LocationNameRec foo;
	PortInfoRec bar;
	
	PPCBrowser("\pChoose an Xconq game to join:",
			   "\pXconq", 0, &foo, &bar, NULL, "\p?");
#endif

static pascal Boolean
filter_warning_alert(DialogPtr dialog, EventRecord *evt, short *itemhit)
{
	char ch;

	/* Look for the right kind of event. */
	switch (evt->what) {
		case keyDown:
			ch = evt->message & charCodeMask;
			if (ch == 3 || ch == 13) {
				*itemhit = 1;
				return TRUE;
			}
			break;
	}
	if (evt->modifiers & optionKey)
	  warnings_suppressed = TRUE;
	return FALSE;
}

/* A warning just gets displayed, no other action is taken. */

void
low_init_warning(char *str)
{
	Str255 buf;

	/* Cursor may be weird from loading, reset it. */
	SetCursor(&QD(arrow));
	c2p(str, buf);
	ParamText(buf, "\p", "\p", "\p");
	switch (CautionAlert(aInitWarning, filter_warning_alert_proc)) {
		case 1:
			/* Just keep going, player considers warning a false alarm. */
			break;
		case 2:
			/* It would be better to undo everything and blast back to initial choices,
			   but that would be pretty hard to implement, and should be a rare occurrence
			   anyway. */
			ExitToShell();
			break;
	}
}

/* An init error is not necessarily fatal, but we still have to start over. */

void
low_init_error(char *str)
{
	Str255 buf;

	/* Cursor may be weird from loading, reset it. */
	SetCursor(&QD(arrow));
	c2p(str, buf);
	ParamText(buf, "\p", "\p", "\p");
	StopAlert(aInitError, nil);
	/* This is a bad time to choke, no way to recover.  Fortunately,
	   it's not a big loss, since there's no game yet to lose,
	   and so we can just exit directly. */
	ExitToShell();
}

/* Runtime warnings are for when it's important to bug the players,
   but doesn't necessarily mean imminent danger of a crash. */

void
low_run_warning(char *str)
{
	Str255 buf;

	/* If we're not actually in the game yet, make an init warning instead. */
	if (beforestart) {
		low_init_warning(str);
		return;
	}
	c2p(str, buf);
	ParamText(buf, "\p", "\p", "\p");
	switch (CautionAlert(aRunWarning, filter_warning_alert_proc)) {
		case 1:
			/* Just keep going, player considers warning a false alarm. */
			break;
		case 2:
			save_the_game(TRUE, TRUE);
			ExitToShell();
			break;
		case 3:
			/* Just blast out of here. */
			ExitToShell();
			break;
	}
}

/* An run error is fatal, but allow an emergency save, might be able to salvage. */

void
low_run_error(char *str)
{
	Str255 buf;

	/* If we're not actually in the game yet, make an init error instead. */
	if (beforestart) {
		low_init_error(str);
		return;
	}
	/* Make some space available, in case this is a memory exhaustion error. */
	if (spare != nil) {
		DisposHandle(spare);
		spare = nil;
	}
	c2p(str, buf);
	ParamText(buf, "\p", "\p", "\p");
	switch (StopAlert(aRunError, nil)) {
		case 1:
			break;
		case 2:
			save_the_game(TRUE, TRUE);
			break;
	}
	/* We're outta here - just ahead of scrambled heaps and dangling ptrs! */
	ExitToShell();
}

static FILE *pffp;

static int first_print = TRUE;

void
print_form(form)
Obj *form;
{
	if (pffp == NULL) {
		pffp = fopen("Xconq.PrintOut", (first_print ? "w" : "a"));
		first_print = FALSE;
	}
	print_form_and_value(pffp, form);
	fflush(pffp);
}

void
end_printing_forms()
{
	if (pffp != NULL) {
		fclose(pffp);
		pffp = NULL;
	}
}

/* This is true when a side has a display that may be safely written to. */

int
active_display(Side *side)
{
	return (side && side->ui && side->ui->active);
}

/* The Mac never has any display buffers to flush. */

void
flush_display_buffers(Side *side)
{
}

/* Detect types of windows. */

int
is_da_window(WindowPtr win)
{
	return (win != nil && ((WindowPeek) win)->windowKind < 0);
}

int
is_app_window(WindowPtr win)
{
	return (win != nil && ((WindowPeek) win)->windowKind >= 0);
}

void
low_notify(Side *side, char *str)
{
	if (!active_display(side))
	  return;
	append_notice(str);
}

/* Kernel callback to update info about the given side. */

void
update_side_display(Side *side, Side *side2, int rightnow)
{
	GrafPtr oldport;
	extern int gamenumsides;

	if (active_display(side) && side2 != NULL) {
		GetPort(&oldport);
		if (gamewin != nil && ((WindowPeek) gamewin)->visible) {
			if (gamenumsides == numsides) {
				SetPort(gamewin);
				draw_side_status(side2);
			} else {
				/* A side must have been added recently; redraw
				   the entire window this time. */
				if (eimages[side_number(side2)] == NULL)
				  init_emblem_images(side2);
				draw_game();
			}
		}
		SetPort(oldport);
		if (side2 == dside && !side->ingame && side->status == 0 && wasingame) {
			/* (should be able to quit from here?) */
			CautionAlert(aOutOfGame, nil); 
			wasingame = FALSE;
			dside->may_set_see_all = TRUE;
		}
	}
}

/* Kernel callback to show the current turn. */

void
update_turn_display(Side *side, int rightnow)
{
	GrafPtr oldport;
	Map *map;
	extern char *curseasonname;

	if (active_display(side)) {
		strcpy(curdatestr + 1, absolute_date_string(g_turn()));
		if (curseasonname != NULL) {
			strcat(curdatestr + 1, " (");
			strcat(curdatestr + 1, curseasonname);
			strcat(curdatestr + 1, ")");
		}
		curdatestr[0] = strlen(curdatestr + 1);
		GetPort(&oldport);
		if (gamewin != nil && ((WindowPeek) gamewin)->visible) {
			SetPort(gamewin);
			draw_game_date();
		}
		for_all_maps(map) {
			if (map->toplineh > 0) {
				SetPort(map->window);
				draw_top_line(map);
			}
			if (map->topunith > 0) {
				SetPort(map->window);
				draw_unit_info(map);
			}
		}
		/* This routine might have been called because the game is over. */
		if (endofgame && !told_outcome && side == dside) {
			if (side_won(dside)) {
		    	won_game_dialog();
		    } else if (side_lost(dside)) {
		    	lost_game_dialog();
		    } else {
				game_over_dialog();
			}
			told_outcome = TRUE;
		    side->may_set_see_all = TRUE;
		}
		SetPort(oldport);
	}
}

/* Callback that gets run once after all turn setup is done but before any movement. */

void
update_action_display(Side *side, int rightnow)
{
	GrafPtr oldport;
	Map *map;
	List *list;
	UnitCloseup *unitcloseup;

	if (active_display(side)) {
		GetPort(&oldport);
		for_all_maps(map) {
			draw_selections(map);
			if (map->autoselect) {
				unselect_all(map);
				map->curunit = NULL;
			}
		}
		for_all_lists(list) {
			reorganize_list(list);
		}
		for_all_unit_closeups(unitcloseup) {
			force_update(unitcloseup->window);
		}
		SetPort(oldport);
	}
}

void
update_action_result_display(Side *side, Unit *unit, int rslt, int rightnow)
{
	Action *action;

    if (active_display(side)) {
    	action = (unit->act ? &(unit->act->nextaction) : NULL);
    	if (action == NULL)
    	  return;
    	DGprintf("%s %s result is %s\n",
    			unit_desig(unit), action_desig(action), hevtdefns[rslt].name);
    	switch (action->type) {
    		case ACTION_CREATE_IN:
    		case ACTION_CREATE_AT:
    		case ACTION_BUILD:
 				/* If any construction-type action succeeded, we should update
 				   the list of types in the construction window, because the
 				   counts of types might have changed. */
    			if (rslt == A_ANY_DONE) {
    				update_construction_type_list();
    			}
    			break;
    	}
    	update_unit_in_maps(unit);
    }
}

void
update_event_display(Side *side, HistEvent *hevt, int rightnow)
{
	if (active_display(side)) {
		/* Tweak the history window if it's up. */
		if (historywin != nil && ((WindowPeek) historywin)->visible) {
			update_history_window(hevt);
		}
	}
}

void
update_fire_at_display(Side *side, Unit *unit, Unit *unit2, int m, int rightnow)
{
	int i, sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2, dx, dy, xx, yy;
	int startticks, innerticks;
	Map *map;
	GrafPtr oldport, curport = NULL;

	if (active_display(side)) {
		GetPort(&oldport);
		startticks = TickCount();
		i = 0;
		/* Tweak the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenMode(patXor);
			if (map->vp->hw > 10)
			  PenSize(2, 2);
			else
			  PenSize(1, 1);
		}
		while (TickCount() < startticks + 32) {
			innerticks = TickCount();
			for_all_maps(map) {
				if (curport != map->window) {
					SetPort(map->window);
					curport = map->window;
				}
				m_xform_unit_self(map, unit, &sx1, &sy1, &sw1, &sh1);
				m_xform_unit_self(map, unit2, &sx2, &sy2, &sw2, &sh2);
				compute_fire_line_segment(sx1 + sw1 / 2, sy1 + sh1 / 2,
										  sx2 + sw2 / 2, sy2 + sh2 / 2,
										  i, 4, &xx, &yy, &dx, &dy);
				MoveTo(xx, yy);  Line(dx, dy);
			}
			/* 2 here seems a bit slowish */
			while (TickCount() < innerticks + 1)
			  ;
			++i;
		}
		/* Restore the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenNormal();
		}
		SetPort(oldport);
	}
}

void
update_fire_into_display(Side *side, Unit *unit, int x2, int y2, int z2, int m, int rightnow)
{
	int i, sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2, dx, dy, xx, yy;
	int startticks, innerticks;
	Map *map;
	GrafPtr oldport, curport = NULL;

	if (active_display(side)) {
		GetPort(&oldport);
		startticks = TickCount();
		i = 0;
		/* Tweak the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenMode(patXor);
			if (map->vp->hw > 10)
			  PenSize(2, 2);
			else
			  PenSize(1, 1);
		}
		while (TickCount() < startticks + 32) {
			innerticks = TickCount();
			for_all_maps(map) {
				if (curport != map->window) {
					SetPort(map->window);
					curport = map->window;
				}
				m_xform_unit_self(map, unit, &sx1, &sy1, &sw1, &sh1);
				xform(map, x2, y2, &sx2, &sy2);
				compute_fire_line_segment(sx1 + sw1 / 2, sy1 + sh1 / 2,
										  sx2 + map->vp->hw / 2, sy2 + map->vp->hh / 2,
										  i, 4, &xx, &yy, &dx, &dy);
				MoveTo(xx, yy);  Line(dx, dy);
			}
			/* 2 here seems a bit slowish */
			while (TickCount() < innerticks + 1)
			  ;
			++i;
		}
		/* Restore the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenNormal();
		}
		SetPort(oldport);
	}
}

/* Update any displayed info about the given unit. */

void
update_unit_display(Side *side, Unit *unit, int rightnow)
{
	UnitCloseup *unitcloseup;

	if (active_display(side) && unit != NULL) {
		update_unit_in_maps(unit);
		if (1 /* unit visible to side in any way */ && inside_area(unit->x, unit->y)) {
			update_cell_display(side, unit->x, unit->y, TRUE);
		}
		update_unit_in_lists(unit);
		if ((unitcloseup = find_unit_closeup(unit)) != NULL
		    && 1 /* window is visible */) {
			draw_unit_closeup(unitcloseup);
		}
		if (unit->side != NULL && unit->act != NULL) {
			update_side_display(side, unit->side, rightnow);
		}
		if (constructionwin != nil
		    && ((WindowPeek) constructionwin)->visible) {
			update_construction_unit_list(unit);
		}
	}
}

void
update_unit_acp_display(Side *side, Unit *unit, int rightnow)
{
	UnitCloseup *unitcloseup;

	if (active_display(side) && unit != NULL) {
		update_unit_in_maps(unit);
#if 0  /* maybe add later, maybe not - acp change not usually visible tho */
		if (1 /* unit visible to side in any way */ && inside_area(unit->x, unit->y)) {
			update_cell_display(side, unit->x, unit->y, TRUE);
		}
#endif
		update_unit_in_lists(unit);
		unitcloseup = find_unit_closeup(unit);
		if (unitcloseup != NULL && (((WindowPeek) (unitcloseup->window))->visible)) {
			draw_unit_closeup(unitcloseup);
		}
	}
}

void
update_unit_in_maps(Unit *unit)
{
	Map *map;

	if (!side_controls_unit(dside, unit) || !alive(unit)) {
		for_all_maps(map) {
			unselect_unit_on_map(map, unit);
			if (unit == map->curunit) {
				map->curunit = NULL;
			}
		}
		return;
	}
	if (side_controls_unit(dside, unit)) {
		for_all_maps(map) {
			if (map->topunith > 0 && map->numselections == 1 && unit == map->selections[0]) {
				draw_unit_info(map);
			}
		}
	}
}

void
update_clock_display(Side *side, int rightnow)
{
	GrafPtr oldport;
#if 0
	Map *map;
#endif
	time_t now;
	extern time_t lastnow;

	if (active_display(side)) {
		time(&now);
		/* If no changes since the last draw, jump out of here. */
		if (now == lastnow)
		  return;
		GetPort(&oldport);
		if (gamewin != nil && ((WindowPeek) gamewin)->visible) {
			SetPort(gamewin);
			draw_game_clocks();
		}
#if 0  /* no clock display in the topline yet */
		for_all_maps(map) {
			if (map->toplineh > 0) {
				SetPort(map->window);
				draw_top_line(map);
			}
		}
#endif
		SetPort(oldport);
	}
}

void
update_all_progress_displays(char *str, int s)
{
	GrafPtr oldport;
	extern char *game_progress_str;

	if (!active_display(dside))
	  return;
	game_progress_str = str;
	GetPort(&oldport);
	if (gamewin != nil && ((WindowPeek) gamewin)->visible) {
		SetPort(gamewin);
		draw_game_progress();
	}
	SetPort(oldport);
}

/* Bring up a modal dialog saying that the player has won. */

void
won_game_dialog()
{
	int done = FALSE;
	short ditem;
	WindowPtr win;

	win = GetNewDialog(dWinGame, NULL, (DialogPtr) -1L);
	ShowWindow(win);
	while (!done) {
		draw_default_button(win, diWinGameQuit);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diWinGameQuit:
				ExitToShell();
				break;
			case diWinGameContinue:
				done = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(win);
}

/* Bring up a modal dialog saying that the player has lost. */

void
lost_game_dialog()
{
	int done = FALSE;
	short ditem;
	WindowPtr win;
	extern int forcedtoresign;

	/* If quitting required a resignation, the player just
	   wants out, with no dwelling on final position, so
	   bypass the dialog here. */
	if (forcedtoresign) {
		ExitToShell();
		return;
	}
	win = GetNewDialog(dLoseGame, NULL, (DialogPtr) -1L);
	ShowWindow(win);
	while (!done) {
		draw_default_button(win, diLoseGameQuit);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diLoseGameQuit:
				ExitToShell();
				break;
			case diLoseGameContinue:
				done = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(win);
}

/* Bring up a modal dialog saying that the game has ended, with no
   implication of the player having either won or lost. */

void
game_over_dialog()
{
	int done = FALSE;
	short ditem;
	WindowPtr win;

	win = GetNewDialog(dGameOver, NULL, (DialogPtr) -1L);
	ShowWindow(win);
	while (!done) {
		draw_default_button(win, diGameOverQuit);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diGameOverQuit:
				ExitToShell();
				break;
			case diGameOverContinue:
				done = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(win);
}

/* Update the displays to reflect the arrival of a message from another
   side. */

void
update_message_display(Side *side, Side *sender, char *str, int rightnow)
{
	int done = FALSE;
	short ditem;
	Str255 tmpstr;
	WindowPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (active_display(side)) {
		if (str == NULL)
		  str = "";
		if (1 /* chose to make a dialog */) {
			win = GetNewDialog(dMessageReceive, NULL, (DialogPtr) -1L);
			GetDItem(win, diMessageReceiveText, &itemtype, &itemhandle, &itemrect);
			c2p(str, tmpstr);
			SetIText(itemhandle, tmpstr);
			ShowWindow(win);
			while (!done) {
				draw_default_button(win, diMessageReceiveOK);
				SetCursor(&QD(arrow));
				ModalDialog(NULL, &ditem);
				switch (ditem) {
					case diMessageReceiveOK:
						done = TRUE;
						break;
					default:
						break;
				}
			}
			DisposDialog(win);
		} else {
			notify(side, "From %s: %s", side_desig(sender), str);
		}
	}
}

void
action_point(Side *side, int x, int y)
{
	Map *map;

	if (side != dside)
	  return;
	if (!inside_area(x, y))
	  return;

	for_all_maps(map) {
		if (map->follow_action && !in_middle(map, x, y)) {
			set_view_focus(map->vp, x, y);
			m_center_on_focus(map);
			set_map_scrollbars(map);
			force_map_update(map);
		}
	}    
}

/* Support for movie display. */

int
schedule_movie(Side *side, enum movie_type movie, ...)
{
	int i;
	va_list ap;

	if (numscheduled >= 10)
	  return FALSE;
	if (side != dside)
	  return FALSE;
	memset(&(movies[numscheduled]), 0, sizeof(struct a_movie));
	movies[numscheduled].type = movie;
	va_start(ap, movie);
	for (i = 0; i < 5; ++i)
	  movies[numscheduled].args[i] = va_arg(ap, int);
	va_end(ap);
	++numscheduled;
	return TRUE;
}

void
play_movies(SideMask sidemask)
{
	int i, j, unitid, startticks, innerticks;
	Map *map;
	Unit *unit;
	GrafPtr oldport, curport = NULL;
	long startcount;

    if (!side_in_set(dside, sidemask))
      return;
	if (1 /* anything visible */) {
		GetPort(&oldport);
		startticks = TickCount();
		i = 0;
		/* Tweak the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenMode(patXor);
			if (map->vp->hw > 10)
			  PenSize(2, 2);
			else
			  PenSize(1, 1);
		}
		while (TickCount() < startticks + 32) {
			innerticks = TickCount();
			for (j = 0; j < numscheduled; ++j) {
				switch (movies[j].type) {
					case movie_null:
						break;
					case movie_miss:
						unitid = movies[j].args[0];
						unit = find_unit(unitid);
						if (unit == NULL || !in_area(unit->x, unit->y))
						  continue;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							draw_unit_blast(map, unit, 0);
						}
						startcount = TickCount();
						play_sound("crunch");
						/* Delay for part of a second (should relinquish cpu tho) */
						while ((TickCount() - startcount) < 10)
						  ;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							clear_unit_blast(map, unit, 0);
						}
						break;
					case movie_hit:
						unitid = movies[j].args[0];
						unit = find_unit(unitid);
						if (unit == NULL || !in_area(unit->x, unit->y))
						  continue;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							draw_unit_blast(map, unit, 1);
						}
						startcount = TickCount();
						play_sound("crunch");
						play_sound("boom");
						/* Delay for part of a second (should relinquish cpu tho) */
						while ((TickCount() - startcount) < 10)
						  ;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							clear_unit_blast(map, unit, 1);
						}
						break;
					case movie_death:
						unitid = movies[j].args[0];
						unit = find_unit(unitid);
						if (unit == NULL || !in_area(unit->x, unit->y))
						  continue;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							draw_unit_blast(map, unit, 2);
						}
						startcount = TickCount();
						play_sound("crunch");
						play_sound("boom");
						/* Delay for part of a second (should relinquish cpu tho) */
						while ((TickCount() - startcount) < 10)
						  ;
						for_all_maps(map) {
							if (curport != map->window) {
								SetPort(map->window);
								curport = map->window;
							}
							clear_unit_blast(map, unit, 2);
						}
						break;
					case movie_nuke:
						/* ??? */
						break;
					case movie_extra_0:
						/* Just play the given sound. */
						play_sound((char *) movies[j].args[0]);
						break;
					default:
						break;
				}
			}
			while (TickCount() < innerticks + 32)
			  ;
			++i;
		}
		/* Restore the pen modes of all the maps. */
		for_all_maps(map) {
			SetPort(map->window);
			PenNormal();
		}
		SetPort(oldport);
	}
	numscheduled = 0;
}

void
play_sound(char *soundname)
{
	Str255 sndnamebuf;
	Handle sound;

    if (!playsounds || empty_string(soundname) || numsoundplays >= 5)
      return;
	c2p(soundname, sndnamebuf);
	sound = GetNamedResource('snd ', sndnamebuf);
	if (sound != nil) {
		HLock(sound);
		/* We play the sound synchronously, because the time delay is needed to
		   make sure the graphics are visible. */
        SndPlay(nil, (SndListHandle) sound, false);
        HUnlock(sound);
        ReleaseResource(sound);
        ++numsoundplays;
    } else {
		run_warning("No sound named \"%s\" available!", soundname);
		/* (should only complain about each sound once) */
    }
}

/* Move the window to a position staggered from the given last position. */

void
stagger_window(WindowPtr win, int *lasthp, int *lastvp)
{
	int h, v, retry = 0;
	Rect winrect;
	GrafPtr oldport;

	if (*lasthp > 0 && *lastvp > 0) {
		while (1) {
			h = *lasthp + 20;  v = *lastvp + 20;
			/* Let windows go partly off the screen, but keep at least the top
			   40x40 pixels visible. */
			if (!position_on_screen(h + 40, v + 40)) {
				if (retry < 20) {
					h = retry * 20; v = 40;
					++retry;
				} else {
					/* This is getting out of hand - just pick something. */
					h = 20;  v = 40;
					break;
				}
			} else if (!position_already_used(h, v)) {
				break;
			}
		}
		MoveWindow(win, h, v, FALSE);
		*lasthp = h;  *lastvp = v;
	} else {
		/* Don't move the first window, but do record its position. */
		GetPort(&oldport);
		SetPort(win);
		winrect = win->portRect;
		LocalToGlobal(&top_left(winrect));
		*lasthp = winrect.left;  *lastvp = winrect.top;
		SetPort(oldport);
	}
}

/* Return true if the given position is visible on a screen somewhere. */

int
position_on_screen(int h, int v)
{
	Point pnt;
	GDHandle screen;

	pnt.h = h;  pnt.v = v;
	if (hasColorQD) {	
		for (screen = GetDeviceList(); screen != nil; screen = GetNextDevice(screen)) {
			if (TestDeviceAttribute(screen, screenDevice)
				&& TestDeviceAttribute(screen, screenActive)) {
				if (PtInRect(pnt, &((*screen)->gdRect)))
				  return TRUE;
			}
		}
	} else {
		if (PtInRect(pnt, &(QD(screenBits).bounds)))
		  return TRUE;
	}
	return FALSE;
}

/* (should make this more efficient?) */

int
position_already_used(int h, int v)
{
	int i;
	Rect winrect;
	WindowPtr win;
	GrafPtr oldport;
	extern int numwindows;
	extern WindowPtr *winmenuwins;

	for (i = 0; i < numwindows; ++i) {
		win = winmenuwins[i];
		GetPort(&oldport);
		SetPort(win);
		winrect = win->portRect;
		LocalToGlobal(&top_left(winrect));
		SetPort(oldport);
		if (h == winrect.left && v == winrect.top)
		  return TRUE;
	}
	return FALSE;
}

/* (This may only be called if Color Quickdraw is present.) */

GDHandle
best_zoom_screen(Rect *rectptr)
{
	int greatestarea = 0, sectarea;
	Rect srect;
	GDHandle screen = GetDeviceList(), bestscreen = GetMainDevice();

	while (screen != nil) {
		if (TestDeviceAttribute(screen, screenDevice)
			&& TestDeviceAttribute(screen, screenActive)) {
			SectRect(rectptr, &((*screen)->gdRect), &srect);
			sectarea = (srect.right - srect.left) * (srect.bottom - srect.top);
			if (sectarea > greatestarea) {
				greatestarea = sectarea;
				bestscreen = screen;
			}
		}
		screen = GetNextDevice(screen);
	}
	return bestscreen;
}

void
set_standard_state(WindowPtr win, int fullw, int fullh)
{
	int screenw, screenh, wintitlehgt, mbaradj = 0;
	Rect winrect, gdrect, zoomrect;
	GDHandle bestscreen;

	winrect = win->portRect;
	LocalToGlobal((Point *) &(winrect.top));
	LocalToGlobal((Point *) &(winrect.bottom));
	wintitlehgt = winrect.top - 1 - (*(((WindowPeek) win)->strucRgn))->rgnBBox.top;
	if (hasColorQD) {
		/* Get the best screen to zoom on. */
		bestscreen = best_zoom_screen(&winrect);
		gdrect = (*bestscreen)->gdRect;
		/* Adjust to the actual subarea that we can use. */
		if (bestscreen == GetMainDevice()) {
			gdrect.top += GetMBarHeight();
		}
		InsetRect(&gdrect, 3, 3);
		gdrect.top += wintitlehgt;
		screenw = gdrect.right - gdrect.left;  screenh = gdrect.bottom - gdrect.top;
		if (winrect.left + fullw <= gdrect.right
			&& winrect.top + fullh <= gdrect.bottom) {
			SetRect(&zoomrect, winrect.left, winrect.top, winrect.left + fullw, winrect.top + fullh);
		} else if (fullw <= screenw || fullh <= screenh) {
			SetRect(&zoomrect, gdrect.left, gdrect.top, gdrect.left + fullw, gdrect.top + fullh);
			if (fullw > screenw)
			  zoomrect.right = gdrect.right;
			if (fullh > screenh)
			  zoomrect.bottom = gdrect.bottom;
		} else {
			zoomrect = gdrect;
		}
	} else {
		zoomrect = QD(screenBits).bounds;
		zoomrect.top += GetMBarHeight();
		InsetRect(&zoomrect, 4, 4);
		zoomrect.top += wintitlehgt;
	}
	((WStateDataPtr) *(((WindowPeek) win)->dataHandle))->stdState = zoomrect;
}

void
get_main_screen_size(int *widp, int *hgtp)
{
	Rect rect;
	GrafPtr mainport;
	GDHandle mainscreen;

	if (hasColorQD) {
		mainscreen = GetMainDevice();
		rect = (*mainscreen)->gdRect;
	} else {
		GetWMgrPort(&mainport);
		rect = mainport->portRect;
	}
	if (widp)
	  *widp = rect.right - rect.left;
	if (hgtp)
	  *hgtp = rect.bottom - rect.top;
}

/* General routine to outline the given item of a given dialog. */

void
draw_default_button(DialogPtr dialog, short ditem)
{
	GrafPtr oldport;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetPort(&oldport);
	SetPort(dialog);
	GetDItem(dialog, ditem, &itemtype, &itemhandle, &itemrect);
	PenSize(3, 3);
	InsetRect(&itemrect, -4, -4);
	FrameRoundRect(&itemrect, 16, 16);
	PenNormal();
	SetPort(oldport);
}

char *
get_string_from_item(Handle itemhandle)
{
	char tmpbuf[BUFSIZE];
	Str255 tmpstr;
	
	GetIText(itemhandle, tmpstr);
	p2c(tmpstr, tmpbuf);
	return copy_string(tmpbuf);
}

/* Cause an update of a window's entire contents. */

void
force_update(WindowPtr win)
{
	GrafPtr oldport;

	if (win == nil)
	  return;
	GetPort(&oldport);
	SetPort(win);
	EraseRect(&win->portRect);
	InvalRect(&win->portRect);
	SetPort(oldport);
}

void
force_overall_update()
{
	Map *map;
	List *list;
	UnitCloseup *unitcloseup;

	force_update(gamewin);
	force_update(historywin);
	force_update(constructionwin);
	force_update(helpwin);
	force_update(noticewin);
	force_update(commandwin);
	force_update(scoreswin);
	for_all_maps(map) {
		force_update(map->window);
	}
	for_all_lists(list) {
		force_update(list->window);
	}
	for_all_unit_closeups(unitcloseup) {
		force_update(unitcloseup->window);
	}
}

void
beep()
{
	SysBeep(20);
}

void
update_everything()
{
	if (active_display(dside)) {
		force_overall_update();
	}
}

/* Set the type and creator of the file to be what is expected
   for a game design. */

void
set_game_file_type(char *name)
{
	FileParam pb;
	Str255 tmpstr;
	
	c2p(name, tmpstr);
	pb.ioNamePtr = tmpstr;
	pb.ioVRefNum = 0;
	pb.ioFVersNum = 0;
	pb.ioFDirIndex = 0;
	if (PBGetFInfoSync((ParmBlkPtr) &pb) == noErr) {
		pb.ioFlFndrInfo.fdType = 'TEXT';
		pb.ioFlFndrInfo.fdCreator = XconqSignature;
		PBSetFInfoSync((ParmBlkPtr) &pb);
	}
}

/* Low-level transmission to another program. */

void
low_send(int id, char *buf)
{
	Dprintf("Sent: %d \"%s\"\n", id, (buf ? buf : "(null)"));
    switch (connection_method) {
		case 0:
			break;
		case 1:
			break;
		case 9:
			break;
		default:
			break;
    }
}

int
low_receive(int *id, char *buf, int maxchars, int timeout)
{
    int rslt = FALSE;

    switch (connection_method) {
		case 0:
			break;
		case 1:
			break;
		case 9:
			*id = 2;
			if (maxchars > 0 && buf != NULL) {
				strcpy(buf, "OK");
			}
			rslt = TRUE;
			break;
		default:
			break;
    }
    if (rslt)
      Dprintf("Rcvd: %d \"%s\"\n", *id, (buf ? buf : "(null)"));
    return rslt;
}

#ifdef __MWERKS__

/* Empty definitions for Metrowerks' SIOUX console library. */

#ifndef __CONSOLE__
#include <console.h>
#endif

short
InstallConsole(short fd)
{
#pragma unused (fd)
	return 0;
}

void
RemoveConsole(void)
{
}

long
WriteCharsToConsole(char *buf, long n)
{
#pragma unused (buf, n)
	return 0;
}

long ReadCharsFromConsole(char *buf, long n)
{
#pragma unused (buf, n)
	return 0;
}

extern char *
__ttyname(long fd)
{
	static char *__devicename = "null device";

	if (fd >= 0 && fd <= 2)
	  return (__devicename);
	return NULL;
}

#endif /* __MWERKS__ */
