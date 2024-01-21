/* Initialization for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING. */

#include "conq.h"
extern int numremotes;
extern void low_send(int id, char *buf);
extern int low_receive(int *id, char *buf, int maxchars, int timeout);
extern int allbedesigners;
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
#include "macconq.h"

/* Limit on the number of check boxes and sliders in the variants dialog. */

#define MAXCHECKBOXES 7
#define MAXSLIDERS 3

#define toggle_checkbox(ih) \
  SetCtlValue((ControlHandle) (ih), !GetCtlValue((ControlHandle) (ih)))

/* (not quite right - come up with a better test) */

#define game_already_loaded() (numutypes > 0)

#define t_color_defined(t) (hasColorQD && tcolors != NULL && tcolors[t] != NULL)

/* Private functions. */

static pascal Boolean filter_splash(DialogPtr dialog, EventRecord *evt, short *itemhit);
static pascal void draw_game_list(WindowPtr win, short ditem);
static pascal void draw_game_blurb(WindowPtr win, short ditem);
static pascal void draw_game_picture(WindowPtr win, short ditem);
static void center_pict(PicHandle picture, Rect *rect);
static Module *module_from_cell(Cell cell);
static pascal Boolean filter_new_game(DialogPtr dialog, EventRecord *evt, short *itemhit);
static void display_selected_game(void);
static void select_game(void);
static pascal void draw_variant_slider(WindowPtr win, short ditem);
static pascal void draw_variant_help(WindowPtr win, short ditem);
static void set_variant_item(int di, int flag, int flag2, int val);
static void interpret_variants(void);
static void implement_variants(void);
static void special_xform(int x, int y, int *sxp, int *syp);
static pascal void draw_world_picture(WindowPtr win, short ditem);
static pascal Boolean filter_world_setup(DialogPtr dialog, EventRecord *evt, short *itemhit);
static int world_shape_dialog(void);
static int real_time_dialog(void);
static int more_variants_dialog(void);
static pascal void draw_player_setup_list(WindowPtr win, short ditem);
static pascal void draw_player_setup_advantage(WindowPtr win, short ditem);
static int adjust_advantage(Player *player, Side *side, int amt);
static void select_player(int n);
static int player_setup_dialog(void);
static void set_player_setup_button_states(void);
static pascal void draw_progress(WindowPtr win, short ditem);
static void add_default_unit_image(ImageFamily *imf, int u, char *name);
static void add_default_terrain_image(ImageFamily *imf, int t, char *name);
static void add_default_emblem_image(ImageFamily *imf, int e, char *name);

/* Global variables. */

BitMap *bordbitmaps;
BitMap *connbitmaps;

ModalFilterUPP filter_splash_proc;

/* This is the dialog used to select a game to play. */

WindowPtr newgamewin = nil;

ListHandle newgamelist = nil;

TEHandle newgametext = nil;

PicHandle newgamepicture = nil;

static Module *selected_game = NULL;

UserItemUPP draw_game_list_proc;
UserItemUPP draw_game_blurb_proc;
UserItemUPP draw_game_picture_proc;

ModalFilterUPP filter_new_game_proc;

/* True if any variants are available. */

int any_variants;

/* These are true when a standard variant is available to be chosen. */

int vary_world_seen;
int vary_see_all;
int vary_world;
int vary_sequential;
int vary_real_time;

/* This is the dialog used to set the size and shape of the world. */

DialogPtr world_shape_win = nil;

/* These are the values that will be used to set world geometry. */

int new_circumference;
int new_width;
int new_height;
int new_latitude;
int new_longitude;
int new_scale;

Handle hexagon_icon;
Handle cylinder_icon;

UserItemUPP draw_world_picture_proc;

ModalFilterUPP filter_world_setup_proc;

/* This is the dialog used to set realtime limits. */

DialogPtr real_time_win = nil;

int new_time_for_game;
int new_time_per_side;
int new_time_per_turn;

/* This is the dialog used to set more variants. */

DialogPtr more_variants_win = nil;

/* This is the variant-setting dialog. */

DialogPtr variants_win = nil;

int numcheckboxes;
int numsliders;

Variant *checkboxes[MAXCHECKBOXES];
Variant *sliders[MAXSLIDERS];

/* These are the new user-chosen values for variants. */

int new_seen;
int new_seeall;
int new_sequential;

Obj *variants;

UserItemUPP draw_variant_slider_proc;
UserItemUPP draw_variant_help_proc;

/* This is the player setup dialog. */

WindowPtr playersetupwin = nil;

PicHandle updownpicture = nil;
PicHandle updownpictureup = nil;
PicHandle updownpicturedown = nil;

int playerh = 22;
int playerbaseline = 14;

int selectedplayer = -1;

#define selected_a_player() (between(0, selectedplayer, numsides - 1))

#define selected_player()  \
  (selected_a_player() ? assignments[selectedplayer].player : NULL)

#define selected_side() (selected_a_player() ? assignments[selectedplayer].side : NULL)

UserItemUPP draw_player_setup_list_proc;
UserItemUPP draw_player_setup_advantage_proc;

/* Caches of images and colors. */

ImageFamily **uimages = NULL;
ImageFamily **timages = NULL;
ImageFamily **eimages = NULL;

ImageColor **tcolors = NULL;

Pattern *animation_patterns;

CursHandle readprogressors[NUMcParens];
CursHandle progressors[NUMcSynth];
CursHandle movecursors[NUMcMoves];
CursHandle nomovecursor;
CursHandle allmovecursor;
CursHandle grayarrowcursor;
CursHandle opencrosscursor;
CursHandle firecursor;
CursHandle watchcursor;

/* This value is the current progress cursor. */

int curcurs = 0;

/* The initialization progress dialog. */

DialogPtr progresswin = nil;

UserItemUPP draw_progress_proc;

/* The current value of the percent progress. */

int progress;

/* The previous value of the percent progress. */

int lastprogress;

enum grays gridgray = whitegray;
enum grays unseengray = whitegray;
enum grays bggray = mediumgray;

RGBColor gridcolor;
RGBColor unseencolor;
RGBColor blackcolor;

int grid_matches_unseen = FALSE;

PicHandle dotdotdotpicture = nil;

int first_windows;

/* Set up any generic patterns that will be needed. */

void
init_patterns()
{
	int i, j, k;
	Handle animpat;

    animation_patterns = (Pattern *) xmalloc(8 * sizeof(Pattern));
	animpat = GetResource('PAT#', 128);
	HLock(animpat);
	k = 0;
	for (i = 0; i < 8; ++i) {
		for (j = 0; j < 8; ++j) {
			SET_PAT_ELT(animation_patterns[i], j, (*animpat)[k++]);
		}
	}
	HUnlock(animpat);
}

/* Set up any generic icons that will be needed. */

void
init_icons()
{
	hexagon_icon = GetResource('ICON', 128);
	cylinder_icon = GetResource('ICON', 129);
}

/* Set up the various cursors we'll be using. */

void
init_cursors()
{
	int i;

	/* Get all the cursors used to indicate reader progress. */
	for (i = 0; i < NUMcParens; ++i) {
		readprogressors[i] = GetCursor(cParens1 + i);
	}
	/* Get all the cursors used to indicate synthesis progress. */
	for (i = 0; i < NUMcSynth; ++i) {
		progressors[i] = GetCursor(cSynth1 + i);
	}
	/* Get all the cursors indicating the move direction. */
	for (i = 0; i < NUMcMoves; ++i) {
		movecursors[i] = GetCursor(cMove1 + i);
	}
	/* Miscellaneous cursors. */
	nomovecursor = GetCursor(cNoMove);
	allmovecursor = GetCursor(cAllMove);
	grayarrowcursor = GetCursor(cGrayArrow);
	opencrosscursor = GetCursor(cOpenCross);
	firecursor = GetCursor(138);
	watchcursor = GetCursor(watchCursor);
	/* Designer-related cursors will be done later, if needed at all. */

	dotdotdotpicture = (PicHandle) GetResource('PICT', 134);
}

static pascal Boolean
filter_splash(DialogPtr dialog, EventRecord *evt, short *itemhit)
{
	char ch;

	/* Look for the right kind of event. */
	switch (evt->what) {
		case keyDown:
			ch = evt->message & charCodeMask;
			if (ch == 3 || ch == 13) {
				*itemhit = diSplashNew;
				return TRUE;
			}
#ifdef DEBUGGING
			/* A secret way to get debugging at startup. */
			/* (should have some sort of feedback?) */
			if (ch == 'D') {
				toggle_debugging(&Debug);
			}
			if (ch == 'M') {
				toggle_debugging(&DebugM);
			}
			if (ch == 'G') {
				toggle_debugging(&DebugG);
			}
			if (ch == 'P') {
				toggle_profiling();
			}
#endif
			break;
	}
	return FALSE;
}

/* Display the initial splash screen, and let the player choose New/Open/Connect/Quit. */

int
do_splash_box()
{
	short ditem;
	Str255 tmpstr;
	WindowPtr win;
	PicHandle pic;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (filter_splash_proc == NULL)
      filter_splash_proc = NewModalFilterProc(filter_splash);

	win = GetNewDialog(dSplash, NULL, (DialogPtr) -1L);
	/* Fill in the kernel's version and copyright. */
	GetDItem(win, diSplashVersion, &itemtype, &itemhandle, &itemrect);
	c2p(version_string(), tmpstr);
	SetIText(itemhandle, tmpstr);
	GetDItem(win, diSplashCopyright, &itemtype, &itemhandle, &itemrect);
	c2p(copyright_string(), tmpstr);
	SetIText(itemhandle, tmpstr);
	/* Substitute a color picture if possible. */
	if (hasColorQD) {
		GetDItem(win, diSplashPicture, &itemtype, &itemhandle, &itemrect);
		pic = (PicHandle) GetResource('PICT', pSplashColor);
		if (pic != nil) {
			SetDItem(win, diSplashPicture, itemtype, (Handle) pic, &itemrect);
		}
	}
	ShowWindow(win);
	SelectWindow(win);
	ModalDialog(filter_splash_proc, &ditem);
	/* We don't loop around here, just return the ditem and let caller decide what to do. */
	DisposDialog(win);
	return ditem;
}

/* Dialog app-defined item callback that displays the list of possible games. */

static pascal void
draw_game_list(WindowPtr win, short ditem)
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Draw the list of available games. */
	LUpdate(newgamewin->visRgn, newgamelist);
	/* Frame it nicely. */
	--itemrect.left;
	FrameRect(&itemrect);
}

/* Dialog app-defined item callback to display the game description. */

static pascal void
draw_game_blurb(WindowPtr win, short ditem)
{
	GrafPtr oldport;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Make sure the text is up-to-date. */
	GetPort(&oldport);
	SetPort(newgamewin);
	TEUpdate(&itemrect, newgametext);	
	SetPort(oldport);
	/* Frame it. */
	InsetRect(&itemrect, -1, -1);
	FrameRect(&itemrect);
}

/* Dialog app-defined item callback to display the game picture, if available. */

static pascal void
draw_game_picture(WindowPtr win, short ditem)
{
	RgnHandle tmprgn;
	Rect cliprect;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	EraseRect(&itemrect);
	if (newgamepicture != nil) {
		tmprgn = NewRgn();
		GetClip(tmprgn);
		cliprect = itemrect;
		ClipRect(&cliprect);
		center_pict(newgamepicture, &itemrect);
		DrawPicture(newgamepicture, &itemrect);
		SetClip(tmprgn);
		DisposeRgn(tmprgn);
	}
#ifdef DESIGNERS
	/* A low-impact feedback that this game will launch directly into designing. */
	if (allbedesigners) {
		PenSize(3, 3);
		itemrect.right -= 3;  itemrect.bottom -= 3;
		FrameRect(&itemrect);
		PenNormal();
	}
#endif /* DESIGNERS */
}

/* Utility that figures out how to center a picture in a rectangle. */

void
center_pict(PicHandle picture, Rect *rect)
{
	int picth, pictv;
	Rect initrect, pictrect;

	initrect = *rect;
	pictrect = (*picture)->picFrame;
	picth = pictrect.right - pictrect.left;  pictv = pictrect.bottom - pictrect.top;
	rect->left += (initrect.right - initrect.left) / 2 - picth / 2;
	rect->top += (initrect.bottom - initrect.top) / 2 - pictv / 2;
	rect->right = rect->left + picth;
	rect->bottom = rect->top + pictv;
}

static Module *
module_from_cell(Cell cell)
{
	return (cell.v < numgames ? possible_games[cell.v] : NULL);
}

/* This filter for the new game dialog just handles the game list, updating the
   other items to reflect the currently selected game. */

int lastnewgameclick;
Point lastnewgamemouse;

static pascal Boolean
filter_new_game(DialogPtr dialog, EventRecord *evt, short *itemhit)
{
	GrafPtr oldport;
	Point pt, origpt;
	short ditem;
	char ch;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	/* Look for the right kind of event. */
	switch (evt->what) {
		case mouseDown:
			GetPort(&oldport);
			SetPort(dialog);
			pt = origpt = evt->where;
			GlobalToLocal(&pt);
			ditem = FindDItem(dialog, pt) + 1;
			if (ditem == diNewGameList) {
				/* Loop around in list handling. */
				LClick(pt, evt->modifiers, newgamelist);
				/* We're finished clicking, show the results (if not in the list's scrollbar). */
				GetDItem(dialog, ditem, &itemtype, &itemhandle, &itemrect);
				if (pt.h < (itemrect.right - sbarwid)) {
					select_game();
					display_selected_game();
					SetPort(oldport);
					if (TickCount() - lastnewgameclick < GetDblTime()
						&& between(-3, origpt.h - lastnewgamemouse.h, 3)
						&& between(-3, origpt.v - lastnewgamemouse.v, 3)
						) {
						*itemhit = diNewGameOK;
					} else {
						lastnewgameclick = TickCount();
						lastnewgamemouse = evt->where;
						*itemhit = diNewGameList;
					}
				}
				return TRUE;
			} else {
				SetPort(oldport);
				return FALSE;
			}
			break;
		case keyDown:
			ch = evt->message & charCodeMask;
			if (ch == 3 || ch == 13) {
				if (selected_game) {
					*itemhit = diNewGameOK;
					return TRUE;
				}
			}
#ifdef DESIGNERS
			/* A secret way to get into designing at startup. */
			if (ch == 'd') {
				allbedesigners = !allbedesigners;
				DrawDialog(newgamewin);
			}
#endif
#ifdef DEBUGGING
			/* A secret way to get debugging at startup. */
			if (ch == 'D') {
				toggle_debugging(&Debug);
				DrawDialog(newgamewin);
			}
			if (ch == 'M') {
				toggle_debugging(&DebugM);
				DrawDialog(newgamewin);
			}
			if (ch == 'G') {
				toggle_debugging(&DebugG);
				DrawDialog(newgamewin);
			}
			if (ch == 'P') {
				toggle_profiling();
				DrawDialog(newgamewin);
			}
#endif
			break;
	}
	return FALSE;
}

/* Given a selected game, display assorted info about it - basically a preview so that
   prospective players can see what they're getting into.  If no game has been selected,
   then this routine clears the displays. */

static void
display_selected_game()
{
	short itemtype;  Handle itemhandle;  Rect itemrect;
	char *desc;
	Str255 tmpstr;
	PicHandle oldnewgamepicture;

	DGprintf("display selected game %s\n", module_desig(selected_game));
	/* Set the blurb for the selected game. */
	GetDItem(newgamewin, diNewGameBlurb, &itemtype, &itemhandle, &itemrect);
	TESetSelect(0, 100000, newgametext);
	TECut(newgametext);
	if (selected_game /* && selected_game->complete */) {
		if (selected_game->blurb) {
			desc = selected_game->blurb;
		} else {
			desc = "??? experimental ???";
		}
	} else {
		desc = "";
	}
	TESetText(desc, strlen(desc), newgametext);
	TEUpdate(&itemrect, newgametext);
	/* Load a picture if one can be found. */
	/* (should look for picture explicitly named in a game-module slot first) */
	oldnewgamepicture = newgamepicture;
	newgamepicture = nil;
	if (selected_game) {
		sprintf(spbuf, "%s game", selected_game->name);
		c2p(spbuf, tmpstr);
		newgamepicture = (PicHandle) GetNamedResource('PICT', tmpstr);
		if (newgamepicture == nil) {
			sprintf(spbuf, "%s", selected_game->name);
			c2p(spbuf, tmpstr);
			newgamepicture = (PicHandle) GetNamedResource('PICT', tmpstr);
		}
	} else {
		/* Nothing to display - how boring! */
	}
	/* Gray out the OK button if no game selected. */
	GetDItem(newgamewin, diNewGameOK, &itemtype, &itemhandle, &itemrect);
	HiliteControl((ControlHandle) itemhandle, (selected_game ? 0 : 255));
	/* We have to force redraw to get the picture item updated. */
	if (oldnewgamepicture != newgamepicture)
	  DrawDialog(newgamewin);
}

/* This is a modal dialog from which the user selects a game and sets options. */

void
new_game_dialog()
{
	int done = FALSE;
	short ditem, i;
	Point cellsize;
	Cell tmpcell;
	Rect listrect, destrect, viewrect;
	Module *module;
	char *gamename;
	char tmpbuf[255];
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (filter_new_game_proc == NULL)
      filter_new_game_proc = NewModalFilterProc(filter_new_game);
	if (draw_game_list_proc == NULL)
	  draw_game_list_proc = NewUserItemProc(draw_game_list);
	if (draw_game_blurb_proc == NULL)
	  draw_game_blurb_proc = NewUserItemProc(draw_game_blurb);
	if (draw_game_picture_proc == NULL)
	  draw_game_picture_proc = NewUserItemProc(draw_game_picture);

	collect_possible_games();
	if (newgamewin == nil) {
		newgamewin = GetNewDialog(dNewGame, NULL, (DialogPtr) -1);
		SetPort(newgamewin);
		TextFont(newYork);
/*		TextSize(10); */
		/* Set up the app-defined item that lists games. */
		GetDItem(newgamewin, diNewGameList, &itemtype, &itemhandle, &itemrect);
		SetDItem(newgamewin, diNewGameList, itemtype, (Handle) draw_game_list_proc, &itemrect);
		SetPt(&cellsize, 0, 0);
		listrect.top = 0;  listrect.left = 0;
		listrect.bottom = 0;  listrect.right = 1;
		itemrect.top += 1;
		itemrect.bottom -= 1;  itemrect.right -= sbarwid + 1;
		/* Create the list of games itself and fill it in. */
		newgamelist = LNew(&itemrect, &listrect, cellsize, 0, newgamewin, 0,0,0,TRUE);
		SetPt(&tmpcell, 0, 0);
		for (i = 0; i < numgames; ++i) {
			module = possible_games[i];
			gamename = (module->title ? module->title : module->name);
			sprintf(tmpbuf, "%s%s", (module->basemodulename ? "-  " : ""), gamename);
			LAddRow(1, tmpcell.v, newgamelist);
			LSetCell(tmpbuf, strlen(tmpbuf), tmpcell, newgamelist);
			++tmpcell.v;
		}
		GetDItem(newgamewin, diNewGameBlurb, &itemtype, &itemhandle, &itemrect);
		SetDItem(newgamewin, diNewGameBlurb, itemtype, (Handle) draw_game_blurb_proc, &itemrect);
		destrect = itemrect;
		viewrect = itemrect;
/*		TextSize(10);  */
		newgametext = TENew(&destrect, &viewrect);
		GetDItem(newgamewin, diNewGamePicture, &itemtype, &itemhandle, &itemrect);
		SetDItem(newgamewin, diNewGamePicture, itemtype, (Handle) draw_game_picture_proc, &itemrect);
		newgamepicture = nil;
	}
	update_new_game_list();
	SetPt(&tmpcell, 0, 0);
	LSetSelect(TRUE, tmpcell, newgamelist);  /* (do this only for new/changed list?) */
	select_game();
	display_selected_game();
	SetCursor(&QD(arrow));
	ShowWindow(newgamewin);
	LDoDraw(1, newgamelist);
	/* Loop around here until the player picks a game to play, or cancels. */
	while (!done) {
		draw_default_button(newgamewin, diNewGameOK);
		ModalDialog(filter_new_game_proc, &ditem);
		switch (ditem) {
			case diNewGameOK:
				/* Make this dialog disappear now, startup may take a long time. */
				HideWindow(newgamewin);
				if (start_new_game()) {
					done = TRUE;
				} else {
					/* If we failed to start the new game, just go around again. */
					ShowWindow(newgamewin);
					SelectWindow(newgamewin);
				}
				break;
			case diNewGameCancel:
				done = TRUE;
				break;
			default:
				break;
		}
	}
	/* We're done, OK to throw away the dialog. */
	DisposDialog(newgamewin);
	newgamewin = nil;
	/* (should release TEs and lists also) */
}

/* Select a game from the list of possible games. */

static void
select_game()
{
	Cell tmpcell;

	SetPt(&tmpcell, 0, 0);
	if (game_already_loaded()) {
		/* selected_game is already set correctly. */
	} else if (LGetSelect(TRUE, &tmpcell, newgamelist)) {
		selected_game = module_from_cell(tmpcell);
	} else {
		selected_game = NULL;
	}
}

/* When the new game OK button is hit, this will do the work of getting things started,
   possibly pausing to ask about player/side setup. */

int
start_new_game()
{
	if (selected_game == NULL) { /* Just in case... */
		beep();
		return FALSE;
	}
	if (!game_already_loaded()) {
		/* Suck in the selected module.  This step cannot be undone. */
		mainmodule = selected_game;
		load_game_module(selected_game, TRUE);
		/* Change cursor back, in case it was different during loading. */
		SetCursor(&QD(arrow));
		/* If the loaded game is not valid, we will get an alert somewhere in here,
		   and possibly bomb out if the player chooses not to continue. */
		check_game_validity();
	}
	return launch_game();
}

/* This routine is for when we end up back in the new game dialog,
   after something has already been loaded. */

/* (could also include any game that has this one as a base module) */

static void
update_new_game_list()
{
	char *gamename;
	Point tmpcell;
	
	if (newgamelist != nil && game_already_loaded()) {
		SetPt(&tmpcell, 0, 0);
		/* Clear out the entire list. */
		LDelRow(0, 0, newgamelist);
		/* Add the loaded game back. */ 
		LAddRow(1, tmpcell.v, newgamelist);
		/* Find a name to paste into the list. */
		gamename = (selected_game->title ? selected_game->title : selected_game->name);
		/* Set and select the one cell. */
		LSetCell(gamename, strlen(gamename), tmpcell, newgamelist);
		LSetSelect(TRUE, tmpcell, newgamelist);
	}
}

static pascal void
draw_variant_slider(WindowPtr win, short ditem)
{
	Str255 tmpstr;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Always frame. */
	FrameRect(&itemrect);
	if (between(diVariantsFirstSlider, ditem, diVariantsFirstSlider + numsliders - 1)) {
		MoveTo(itemrect.left + 2, itemrect.top + 10);
		c2p(sliders[ditem - diVariantsFirstSlider]->name, tmpstr);
		DrawString(tmpstr);
	} else {
		gray_out_rect(&itemrect);
	}
}

static pascal void
draw_variant_help(WindowPtr win, short ditem)
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Always frame. */
	FrameRect(&itemrect);
}

/* The variants dialog basically handles all the player-settable options defined
   by a game. */

static int
variants_dialog()
{
	int i, done = FALSE, changed = FALSE;
	char *gamename;
	Str255 tmpstr;
	short ditem;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (selected_game == NULL)
	  init_error("variants on a nonexistent game?");
	interpret_variants();
	/* If no variants defined, get out of here. */
	if (!any_variants)
	  return TRUE;

	if (draw_variant_slider_proc == NULL)
	  draw_variant_slider_proc = NewUserItemProc(draw_variant_slider);
	if (draw_variant_help_proc == NULL)
	  draw_variant_help_proc = NewUserItemProc(draw_variant_help);

	if (variants_win == nil) {
		variants_win = GetNewDialog(dVariants, NULL, (DialogPtr) -1L);
		/* Set the title of this dialog appropriately. */
		gamename = (selected_game->title ? selected_game->title : selected_game->name);
		sprintf(spbuf, "Variants for \"%s\"", gamename);
		c2p(spbuf, tmpstr);
		GetDItem(variants_win, diVariantsText, &itemtype, &itemhandle, &itemrect);
		SetIText(itemhandle, tmpstr);
		/* Give all the slider app items the same proc. */
		for (i = diVariantsFirstSlider; i < (diVariantsFirstSlider + MAXSLIDERS); ++i) {
			GetDItem(variants_win, i, &itemtype, &itemhandle, &itemrect);
			SetDItem(variants_win, i, itemtype, (Handle) draw_variant_slider_proc, &itemrect);
		}
		GetDItem(variants_win, diVariantsHelp, &itemtype, &itemhandle, &itemrect);
		SetDItem(variants_win, diVariantsHelp, itemtype, (Handle) draw_variant_help_proc, &itemrect);
	}
	/* Display/default the standard variants appropriately. */
	set_variant_item(diVariantsWorldSeen, vary_world_seen, TRUE, new_seen);
	set_variant_item(diVariantsSeeAll, vary_see_all, TRUE, new_seeall);
	set_variant_item(diVariantsSequential, vary_sequential, TRUE, new_sequential);
	set_variant_item(diVariantsWorldSize, vary_world, FALSE, 0);
	set_variant_item(diVariantsRealTime, vary_real_time, FALSE, 0);
	set_variant_item(diVariantsMoreVariants, /* more_variants */ FALSE, FALSE, 0);
	/* For each random checkbox used, give it a title and make it displayable,
	   otherwise gray it. */
	for (i = 0; i < MAXCHECKBOXES; ++i) {
		GetDItem(variants_win, diVariantsFirstCheckBox + i, &itemtype, &itemhandle, &itemrect);
		if (i < numcheckboxes) {
			c2p(checkboxes[i]->name, tmpstr);
			SetCTitle((ControlHandle) itemhandle, tmpstr);
			ShowControl((ControlHandle) itemhandle);
			/* should set its initial state based on variant's default */
		} else {
			sprintf(spbuf, "Unused");
			c2p(spbuf, tmpstr);
			SetCTitle((ControlHandle) itemhandle, tmpstr);
			ShowControl((ControlHandle) itemhandle);
			HiliteControl((ControlHandle) itemhandle, 255);
		}
	}
	ShowWindow(variants_win);
	while (!done) {
		draw_default_button(variants_win, diVariantsOK);
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diVariantsOK:
				implement_variants();
				changed = TRUE;
				/* Fall through to next case. */
			case diVariantsCancel:
				done = TRUE;
				break;
			case diVariantsWorldSeen:
			case diVariantsSeeAll:
			case diVariantsSequential:
				/* Toggle check boxes. */
				GetDItem(variants_win, ditem, &itemtype, &itemhandle, &itemrect);
				toggle_checkbox(itemhandle);
				break;
			case diVariantsWorldSize:
				/* Fire up a separate dialog for world geometry. */
				world_shape_dialog();
				break;
			case diVariantsRealTime:
				/* Fire up a separate dialog for real time setup. */
				real_time_dialog();
				break;
			case diVariantsMoreVariants:
				more_variants_dialog();
				break;
			default:
				/* Handle all the checkboxes similarly. */
				if (between(diVariantsFirstCheckBox, ditem, diVariantsFirstCheckBox+MAXCHECKBOXES-1)) {
					GetDItem(variants_win, ditem, &itemtype, &itemhandle, &itemrect);
					toggle_checkbox(itemhandle);
				}
				break;
		}
	}
	DisposDialog(variants_win);
	variants_win = nil;
	return changed;
}

static void
set_variant_item(di, flag, flag2, val)
int di;
int flag, flag2, val;
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(variants_win, di, &itemtype, &itemhandle, &itemrect);
	ShowControl((ControlHandle) itemhandle);
	HiliteControl((ControlHandle) itemhandle, (flag ? 0 : 255));
	if (flag && flag2) {
		SetCtlValue((ControlHandle) itemhandle, val);
	}
}

/* Go through all the game's variants and set up appropriate flags. */

static void
interpret_variants()
{
	int i;
	char *vartypename;
	Obj *vartmp;
	Variant *var;

	any_variants = FALSE;
	vary_world_seen = vary_see_all = vary_sequential = FALSE;
	vary_world = vary_real_time = FALSE;
	if (selected_game == NULL || selected_game->variants == NULL)
	  return;
	numcheckboxes = 0;
	numsliders = 0;
	for (i = 0; selected_game->variants[i].id != lispnil; ++i) {
		var = &(selected_game->variants[i]);
		any_variants = TRUE;
		vartypename = c_string(var->id);
		switch (keyword_code(vartypename)) {
			case K_WORLD_SEEN:
				vary_world_seen = TRUE;
				new_seen = FALSE;
				if (var->dflt != lispnil) {
					vartmp = eval(var->dflt);
					if (numberp(vartmp)) {
						new_seen = c_number(vartmp);
					}
				}
				break;
			case K_SEE_ALL:
				vary_see_all = TRUE;
				new_seeall = FALSE;
				if (var->dflt != lispnil) {
					vartmp = eval(var->dflt);
					if (numberp(vartmp)) {
						new_seeall = c_number(vartmp);
					}
				}
				break;
			case K_SEQUENTIAL:
				vary_sequential = TRUE;
				new_sequential = FALSE;
				if (var->dflt != lispnil) {
					vartmp = eval(var->dflt);
					if (numberp(vartmp)) {
						new_sequential = c_number(vartmp);
					}
				}
				break;
			case K_WORLD_SIZE:
				vary_world = TRUE;
				/* Start with some defaults. */
				new_circumference = DEFAULTCIRCUMFERENCE;
				new_width = DEFAULTWIDTH;  new_height = DEFAULTHEIGHT;
				new_latitude = 0;  new_longitude = 0;
				/* If we have explicit defaults, use them. */
				if (var->dflt != lispnil) {
					vartmp = var->dflt;
					new_width = c_number(eval(car(vartmp)));
					vartmp = cdr(vartmp);
					if (vartmp != lispnil) {
						new_height = c_number(eval(car(vartmp)));
						vartmp = cdr(vartmp);
					} else {
						new_height = new_width;
					}
					if (vartmp != lispnil) {
						new_circumference = c_number(eval(car(vartmp)));
						vartmp = cdr(vartmp);
					}
					if (vartmp != lispnil) {
						new_latitude = c_number(eval(car(vartmp)));
						vartmp = cdr(vartmp);
					}
					if (vartmp != lispnil) {
						new_longitude = c_number(eval(car(vartmp)));
					}
				}
				break;
			case K_REAL_TIME:
				vary_real_time = TRUE;
				/* Start with some defaults. */
				new_time_for_game = new_time_per_side = new_time_per_turn = 0;
				/* If we have explicit defaults, use them. */
				if (var->dflt != lispnil) {
					vartmp = var->dflt;
					new_time_for_game = c_number(eval(car(vartmp)));
					vartmp = cdr(vartmp);
					if (vartmp != lispnil) {
						new_time_per_side = c_number(eval(car(vartmp)));
					} else {
						new_time_per_side = 0;
					}
					vartmp = cdr(vartmp);
					if (vartmp != lispnil) {
						new_time_per_turn = c_number(eval(car(vartmp)));
					} else {
						new_time_per_turn = 0;
					}
				}
				break;
			default:
				if (1 /* boolean variant */) {
					if (numcheckboxes >= MAXCHECKBOXES) {
						init_warning("too many variants, can't set all of them");
						break;
					}
					checkboxes[numcheckboxes++] = var;
				} else {
					/* (should set up slider) */
				}
				break;
		}
	}
}

/* This is where we actually change the state of the game according to the variants. */

static void
implement_variants()
{
	int i;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	variants = lispnil;
	if (vary_world_seen) {
		GetDItem(variants_win, diVariantsWorldSeen, &itemtype, &itemhandle, &itemrect);
		push_key_int_binding(&variants, K_WORLD_SEEN, GetCtlValue((ControlHandle) itemhandle));
	}
	if (vary_see_all) {
		GetDItem(variants_win, diVariantsSeeAll, &itemtype, &itemhandle, &itemrect);
		push_key_int_binding(&variants, K_SEE_ALL, GetCtlValue((ControlHandle) itemhandle));
	}
	if (vary_sequential) {
		GetDItem(variants_win, diVariantsSequential, &itemtype, &itemhandle, &itemrect);
		push_key_int_binding(&variants, K_SEQUENTIAL, GetCtlValue((ControlHandle) itemhandle));
	}
	if (vary_world) {
		/* It is critically important that users not be able to reshape already-alloced
		   areas, but do let them know that their request had to be overridden. */
		if (((area.width > 0 && area.width != new_width)
			|| (area.height > 0 && area.height != new_height)
			|| (world.circumference > 0 && world.circumference != new_circumference))
			&& (1 /* some layers (probably) allocated already */)) {
			/* (this is misleading, is an "expected" alert) */
			init_warning("Area dimensions must remain %d x %d, %d around world",
						 area.width, area.height, world.circumference);
			new_width = area.width;  new_height = area.height;
			new_circumference = world.circumference;
		}
		/* Make a world-size-setting and glue it into the list of variants. */
		push_key_cdr_binding(&variants, K_WORLD_SIZE, 
							  cons(new_number(new_width),
							 	  cons(new_number(new_height),
									   cons(new_number(new_circumference),
									        cons(new_number(new_longitude),
									             cons(new_number(new_latitude),
								  	        lispnil))))));
	}
	if (vary_real_time) {
		push_key_cdr_binding(&variants, K_REAL_TIME, 
						 cons(new_number(new_time_for_game),
					      cons(new_number(new_time_per_side),
					       cons(new_number(new_time_per_turn),
							lispnil))));
	}
	/* Implement the random checkbox variants. */
	for (i = 0; i < numcheckboxes; ++i) {
		GetDItem(variants_win, diVariantsFirstCheckBox + i, &itemtype, &itemhandle, &itemrect);
		push_int_binding(&variants, checkboxes[i]->id, GetCtlValue((ControlHandle) itemhandle));
	}
	/* (should implement the random slider variants) */
	do_module_variants(selected_game, variants);
	/* Recheck everything, the variants might have broken something. */
	if (any_variants) {
		check_game_validity();
	}
}

/* Specially adapted version of xform. */

static void
special_xform(int x, int y, int *sxp, int *syp)
{
	*sxp = (x + y / 2 - (new_height / 4)) / new_scale;
	*syp = (new_height - y) / new_scale;
}


/* This is a callback that draws the world and its areas. */

/* (Would be really cool to draw accurate curvature over globe here) */

static pascal void
draw_world_picture(WindowPtr win, short ditem)
{
	int llx, lly, lrx, lry, rx, ry, urx, ury, ulx, uly, lx, ly;
	Point dims, center, center1, center2;
	Rect worldrect, hemirect1, hemirect2, arearect;
	PolyHandle poly;
	RgnHandle two_hemi_rgn, tmprgn;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Make a framed gray background for the world picture. */
	FillRect(&itemrect, QDPat(gray));
	FrameRect(&itemrect);
	/* Alter the itemrect so as to leave a bit of border. */
	InsetRect(&itemrect, 5, 5);
	dims.h = itemrect.right - itemrect.left;  dims.v = itemrect.bottom - itemrect.top;
	center.h = itemrect.left + dims.h / 2;  center.v = itemrect.top + dims.v / 2;
	worldrect = itemrect;
	new_scale = 1;
	if (new_circumference > dims.h || new_width > dims.h)
	  new_scale = 2;
	if (new_circumference > 2 * dims.h || new_width > 2 * dims.h)
	  new_scale = 4;
	/* Draw the whole world as a circle if it's not too big. */
	if (new_circumference < new_width) {
		/* If world circumference smaller than area, world is not being used at all. */
	} else if (new_circumference > 4 * dims.h || new_width > 4 * dims.h) {
		/* World is too big to draw, just make a white box. */
		InsetRect(&itemrect, -4, -4);
		FillRect(&itemrect, QDPat(white));
	} else {
		hemirect1.left = center.h - (new_circumference / new_scale) / 2;
		hemirect1.right = center.h;
		hemirect1.top = center.v - ((new_circumference / new_scale) / 2) / 2;
		hemirect1.bottom = center.v + ((new_circumference / new_scale) / 2) / 2;
		center1.h = hemirect1.left + (hemirect1.right - hemirect1.left) / 2;
		center1.v = hemirect1.top + (hemirect1.bottom - hemirect1.top) / 2;
		EraseOval(&hemirect1);
		FrameOval(&hemirect1);
		hemirect2 = hemirect1;
		OffsetRect(&hemirect2, (new_circumference / new_scale) / 2, 0);
		center2.h = hemirect2.left + (hemirect2.right - hemirect2.left) / 2;
		center2.v = hemirect2.top + (hemirect2.bottom - hemirect2.top) / 2;
		EraseOval(&hemirect2);
		FrameOval(&hemirect2);
		two_hemi_rgn = NewRgn();
		OpenRgn();
		FrameOval(&hemirect1);
		FrameOval(&hemirect2);
		CloseRgn(two_hemi_rgn);
		/* Draw some grid lines. */
		tmprgn = NewRgn();
		GetClip(tmprgn);
		SetClip(two_hemi_rgn);
		/* Latitude lines. */
		MoveTo(hemirect1.left,  center1.v);
		LineTo(hemirect1.right, center1.v);
		MoveTo(hemirect1.left,  center1.v + ((hemirect1.bottom - hemirect1.top) / 2) / 2);
		LineTo(hemirect1.right, center1.v + ((hemirect1.bottom - hemirect1.top) / 2) / 2);
		MoveTo(hemirect1.left,  center1.v + (((hemirect1.bottom - hemirect1.top) / 2) * 173) / 200);
		LineTo(hemirect1.right, center1.v + (((hemirect1.bottom - hemirect1.top) / 2) * 173) / 200);
		MoveTo(hemirect1.left,  center1.v - ((hemirect1.bottom - hemirect1.top) / 2) / 2);
		LineTo(hemirect1.right, center1.v - ((hemirect1.bottom - hemirect1.top) / 2) / 2);
		MoveTo(hemirect1.left,  center1.v - (((hemirect1.bottom - hemirect1.top) / 2) * 173) / 200);
		LineTo(hemirect1.right, center1.v - (((hemirect1.bottom - hemirect1.top) / 2) * 173) / 200);
		MoveTo(hemirect2.left,  center2.v);
		LineTo(hemirect2.right, center2.v);
		MoveTo(hemirect2.left,  center2.v + ((hemirect2.bottom - hemirect2.top) / 2) / 2);
		LineTo(hemirect2.right, center2.v + ((hemirect2.bottom - hemirect2.top) / 2) / 2);
		MoveTo(hemirect2.left,  center2.v + (((hemirect2.bottom - hemirect2.top) / 2) * 173) / 200);
		LineTo(hemirect2.right, center2.v + (((hemirect2.bottom - hemirect2.top) / 2) * 173) / 200);
		MoveTo(hemirect2.left,  center2.v - ((hemirect2.bottom - hemirect2.top) / 2) / 2);
		LineTo(hemirect2.right, center2.v - ((hemirect2.bottom - hemirect2.top) / 2) / 2);
		MoveTo(hemirect2.left,  center2.v - (((hemirect2.bottom - hemirect2.top) / 2) * 173) / 200);
		LineTo(hemirect2.right, center2.v - (((hemirect2.bottom - hemirect2.top) / 2) * 173) / 200);
		/* Longitude lines. */
		MoveTo(center1.h, hemirect1.top);
		LineTo(center1.h, hemirect1.bottom);
		MoveTo(center2.h, hemirect2.top);
		LineTo(center2.h, hemirect2.bottom);
		SetClip(tmprgn);
		DisposeRgn(tmprgn);
	}
	/* should use pair of world disks as clipping region */
	if ((new_width / new_scale) > (itemrect.right - itemrect.left)) {
		FillRect(&itemrect, QDPat(dkGray));
	} else if (new_width == new_circumference) {
		arearect.top = center.v - (new_height / new_scale) / 2;
		arearect.left = center.h - (new_width / new_scale) / 2;
		arearect.bottom = center.v + (new_height / new_scale) / 2;
		arearect.right = center.h + (new_width / new_scale) / 2;
		OffsetRect(&arearect, (new_latitude / new_scale), (new_longitude / new_scale));
		FillRect(&arearect, QDPat(dkGray));
	} else {
		/* Assume that area to be drawn is a hexagon. */
		poly = OpenPoly();		
		special_xform(0 + new_height/2, 0, &llx, &lly);
		MoveTo(llx, lly);
		special_xform(new_width-1, 0, &lrx, &lry);
	 	LineTo(lrx, lry);
		special_xform(new_width-1, new_height/2, &rx, &ry);
		LineTo(rx, ry);
 		special_xform(new_width-1 - new_height/2, new_height-1, &urx, &ury);
		LineTo(urx, ury);
 		special_xform(0, new_height-1, &ulx, &uly);
		LineTo(ulx, uly);
 		special_xform(0, new_height/2, &lx, &ly);
		LineTo(lx, ly);
		LineTo(llx, lly);
		ClosePoly();
		OffsetPoly(poly, center.h - (new_width / 2) / new_scale, center.v - (new_height / 2) / new_scale);
		OffsetPoly(poly, (new_latitude / new_scale), (new_longitude / new_scale));
		FillPoly(poly, QDPat(dkGray));
	}
}

/* This draws an outline shape of the area alone.  If the pen is in inverted mode, then this
   is a very useful rubberbanding routine. */

static void
draw_world_outline(WindowPtr win, short ditem)
{
	int llx, lly, lrx, lry, rx, ry, urx, ury, ulx, uly, lx, ly;
	Point dims, center;
	Rect worldrect, arearect;
	PolyHandle poly;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Alter the itemrect so as to leave a bit of border. */
	InsetRect(&itemrect, 5, 5);
	dims.h = itemrect.right - itemrect.left;  dims.v = itemrect.bottom - itemrect.top;
	center.h = itemrect.left + dims.h / 2;  center.v = itemrect.top + dims.v / 2;
	worldrect = itemrect;
	new_scale = 1;
	if (new_circumference > dims.h || new_width > dims.h)
	  new_scale = 2;
	if (new_circumference > 2 * dims.h || new_width > 2 * dims.h)
	  new_scale = 4;
	/* should use pair of world disks as clipping region */
	if ((new_width / new_scale) > (itemrect.right - itemrect.left)) {
		FrameRect(&itemrect);
	} else if (new_width == new_circumference) {
		arearect.top = center.v - (new_height / new_scale) / 2;
		arearect.left = center.h - (new_width / new_scale) / 2;
		arearect.bottom = center.v + (new_height / new_scale) / 2;
		arearect.right = center.h + (new_width / new_scale) / 2;
		OffsetRect(&arearect, (new_latitude / new_scale), (new_longitude / new_scale));
		FrameRect(&arearect);
	} else {
		/* Assume that area to be drawn is a hexagon. */
		poly = OpenPoly();		
		special_xform(0 + new_height/2, 0, &llx, &lly);
		MoveTo(llx, lly);
		special_xform(new_width-1, 0, &lrx, &lry);
	 	LineTo(lrx, lry);
		special_xform(new_width-1, new_height/2, &rx, &ry);
		LineTo(rx, ry);
 		special_xform(new_width-1 - new_height/2, new_height-1, &urx, &ury);
		LineTo(urx, ury);
 		special_xform(0, new_height-1, &ulx, &uly);
		LineTo(ulx, uly);
 		special_xform(0, new_height/2, &lx, &ly);
		LineTo(lx, ly);
		LineTo(llx, lly);
		ClosePoly();
		OffsetPoly(poly, center.h - (new_width / 2) / new_scale, center.v - (new_height / 2) / new_scale);
		OffsetPoly(poly, (new_latitude / new_scale), (new_longitude / new_scale));
		FramePoly(poly);
	}
}

/* Given a point in the dialog item, figure out what width and height it corresponds to. */

static int
dims_from_point(WindowPtr win, short ditem, Point pt, int *widp, int *hgtp)
{
	Point dims, center;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	dims.h = itemrect.right - itemrect.left;  dims.v = itemrect.bottom - itemrect.top;
	center.h = itemrect.left + dims.h / 2;  center.v = itemrect.top + dims.v / 2;
	/* Compute width and height so that click was on a corner. */
	*widp = (2 * abs(pt.h - center.h) + abs(pt.v - center.v)) * new_scale;
	*hgtp = (2 * abs(pt.v - center.v)) * new_scale;
	return valid_area_shape(*widp, *hgtp, FALSE);
}

/* Grab events that we want to handle specially. */

static pascal Boolean
filter_world_setup(DialogPtr dialog, EventRecord *evt, short *itemhit)
{
	GrafPtr oldport;
	Point pt, nextpt;
	short ditem, ditem2;
	int wid, hgt;
	int drawn = FALSE;
	char ch;
	Str255 tmpstr;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	WindowPtr win = world_shape_win;

	switch (evt->what) {
		case mouseDown:
			GetPort(&oldport);
			SetPort(dialog);
			pt = evt->where;
			GlobalToLocal(&pt);
			ditem = FindDItem(win, pt) + 1;
			/* Get out of here if we're not messing with the picture. */
			if (ditem != diWorldShapePicture) {
				SetPort(oldport);
				return FALSE;
			}
			PenMode(patXor);
			PenPat(QDPat(gray));
			if (dims_from_point(win, ditem, pt, &wid, &hgt)) {
				new_width = wid;  new_height = hgt;
				draw_world_outline(win, ditem);
				drawn = TRUE;
			}
			while (WaitMouseUp()) {
				GetMouse(&nextpt);
				if (!EqualPt(pt, nextpt)) {
					/* Erase last outline. */
					if (drawn) {
						draw_world_outline(win, ditem);
					}
					pt = nextpt;
					if (dims_from_point(win, ditem, pt, &wid, &hgt)) {
						new_width = wid;  new_height = hgt;
						draw_world_outline(win, ditem);
						drawn = TRUE;
					}
				}
			}
			if (drawn) {
				draw_world_outline(win, ditem);
			}
			PenNormal();
			/* Check if we released the mouse button outside the world picture;
			   if so, treat it as a cancel. */
			ditem2 = FindDItem(win, pt) + 1;
			if (ditem == ditem2) {
				if (dims_from_point(win, ditem, pt, &wid, &hgt)) {
					put_number_into_ditem(diWorldShapeWidth, wid);
					put_number_into_ditem(diWorldShapeHeight, hgt);
					/* The outer dialog loop will do the redraw. */
				}
				SetPort(oldport);
				*itemhit = diWorldShapePicture;
				return TRUE;
			} else {
				SetPort(oldport);
			}
			break;
		case keyDown:
			ch = evt->message & charCodeMask;
			/* Recognize returns and enters as equivalent to OK. */
			if (ch == 3 || ch == 13) {
				*itemhit = diWorldShapeOK;
				return TRUE;
			}
			break;
	}
	return FALSE;
}

/* This dialog asks for the shape and size of a world to generate randomly. */

static int
world_shape_dialog()
{
	int done = FALSE, maybechanged = TRUE, changed = FALSE;
	Str255 tmpstr;
	short ditem;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	WindowPtr win;

	if (draw_world_picture_proc == NULL)
	  draw_world_picture_proc = NewUserItemProc(draw_world_picture);
	if (filter_world_setup_proc == NULL)
      filter_world_setup_proc = NewModalFilterProc(filter_world_setup);

	if (world_shape_win == nil) {
		world_shape_win = GetNewDialog(dWorldShape, NULL, (DialogPtr) -1L);
		GetDItem(world_shape_win, diWorldShapePicture, &itemtype, &itemhandle, &itemrect);
		SetDItem(world_shape_win, diWorldShapePicture, itemtype, (Handle) draw_world_picture_proc, &itemrect);
		/* Plug all the starting values into dialog items. */
		win = world_shape_win;
		put_number_into_ditem(diWorldShapeCircumference, new_circumference);
		put_number_into_ditem(diWorldShapeWidth, new_width);
		put_number_into_ditem(diWorldShapeHeight, new_height);
		put_number_into_ditem(diWorldShapeLatitude, new_latitude);
		put_number_into_ditem(diWorldShapeLongitude, new_longitude);
	}
	while (!done) {
		/* adjust items to reflect current status */
		if (maybechanged) {
			/* Dig the values out of the dialog boxes. */
			win = world_shape_win;
			get_number_from_ditem(diWorldShapeCircumference, new_circumference);
			get_number_from_ditem(diWorldShapeWidth, new_width);
			get_number_from_ditem(diWorldShapeHeight, new_height);
			get_number_from_ditem(diWorldShapeLatitude, new_latitude);
			get_number_from_ditem(diWorldShapeLongitude, new_longitude);
			GetDItem(world_shape_win, diWorldShapeIcon, &itemtype, &itemhandle, &itemrect);
			SetDItem(world_shape_win, diWorldShapeIcon, itemtype,
					 (new_circumference == new_width ? cylinder_icon : hexagon_icon), &itemrect);
			DrawDialog(world_shape_win); /* just to do picture item */
			maybechanged = FALSE;
		}
		/* If the proposed shape is valid, enable the OK button. */
		/* (still need to disable returns etc) */
		GetDItem(world_shape_win, diWorldShapeOK, &itemtype, &itemhandle, &itemrect);
		HiliteControl((ControlHandle) itemhandle,
					  (valid_area_shape(new_width, new_height, FALSE) ? 0 : 255));
		draw_default_button(world_shape_win, diWorldShapeOK);
		ModalDialog(filter_world_setup_proc, &ditem);
		switch (ditem) {
			case diWorldShapeOK:
				changed = TRUE;
				/* Fall through to next case. */
			case diWorldShapeCancel:
				done = TRUE;
				break;
			case diWorldShapePicture:
			case diWorldShapeCircumference:
			case diWorldShapeWidth:
			case diWorldShapeHeight:
			case diWorldShapeLatitude:
			case diWorldShapeLongitude:
				maybechanged = TRUE;
				break;
			case diWorldShapeIcon:
				if (new_circumference != new_width)
				  new_circumference = new_width;
				else
				  new_circumference = 360;
				maybechanged = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(world_shape_win);
	world_shape_win = nil;
	return changed;
}

/* This dialog asks for real time parameters of the game. */

static int
real_time_dialog()
{
	int done = FALSE, maybechanged = TRUE, changed = FALSE;
	Str255 tmpstr;
	short ditem;
	short itemtype;  Handle itemhandle;  Rect itemrect;
	WindowPtr win;

	if (real_time_win == nil) {
		real_time_win = GetNewDialog(dRealTime, NULL, (DialogPtr) -1L);
		/* Plug all the starting values into dialog items. */
		win = real_time_win;
		put_number_into_ditem(diRealTimeForGame, new_time_for_game);
		put_number_into_ditem(diRealTimePerSide, new_time_per_side);
		put_number_into_ditem(diRealTimePerTurn, new_time_per_turn);
	}
	while (!done) {
		/* adjust items to reflect current status */
		if (maybechanged) {
			/* Dig the values out of the dialog boxes. */
			win = real_time_win;
			get_time_from_ditem(diRealTimeForGame, new_time_for_game);
			get_time_from_ditem(diRealTimePerSide, new_time_per_side);
			get_time_from_ditem(diRealTimePerTurn, new_time_per_turn);
			maybechanged = FALSE;
		}
		/* If the proposed timings are valid, enable the OK button. */
		/* (still need to disable returns etc) */
		GetDItem(real_time_win, diRealTimeOK, &itemtype, &itemhandle, &itemrect);
		HiliteControl((ControlHandle) itemhandle,
					  (1 ? 0 : 255));
		draw_default_button(real_time_win, diRealTimeOK);
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diRealTimeOK:
				changed = TRUE;
				/* Fall through to next case. */
			case diRealTimeCancel:
				done = TRUE;
				break;
			case diRealTimeForGame:
			case diRealTimePerSide:
			case diRealTimePerTurn:
				maybechanged = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(real_time_win);
	real_time_win = nil;
	return changed;
}

static int
more_variants_dialog()
{
	int done = FALSE, maybechanged = TRUE, changed = FALSE;
	char *gamename;
	Str255 tmpstr;
	short ditem;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (more_variants_win == nil) {
		more_variants_win = GetNewDialog(dMoreVariants, NULL, (DialogPtr) -1L);
		gamename = (selected_game->title ? selected_game->title : selected_game->name);
		sprintf(spbuf, "More Variants for \"%s\"", gamename);
		c2p(spbuf, tmpstr);
		GetDItem(variants_win, diVariantsText, &itemtype, &itemhandle, &itemrect);
		SetIText(itemhandle, tmpstr);
	}
	while (!done) {
		/* adjust items to reflect current status */
		if (maybechanged) {
			/* Dig the values out of the dialog boxes. */
			maybechanged = FALSE;
		}
		draw_default_button(more_variants_win, diMoreVariantsOK);
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diMoreVariantsOK:
				changed = TRUE;
				/* Fall through to next case. */
			case diMoreVariantsCancel:
				done = TRUE;
				break;
			default:
				break;
		}
	}
	DisposDialog(more_variants_win);
	more_variants_win = nil;
	return changed;
}

/* This is a callback that displays the list of sides and their assigned players. */

static pascal void
draw_player_setup_list(WindowPtr win, short ditem)
{
	int i, advantage = -1, numvisentries;
	Rect entryrect, tmprect;
	char *sidetitle, playertitle[BUFSIZE];
	Player *player;
	Side *side;
	RgnHandle tmprgn;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	/* Default to normal size and spacing. */
	playerh = 22;
	playerbaseline = 14;
	numvisentries = (itemrect.bottom - itemrect.top) / playerh;
	/* If many sides, double our space by squashing things down. */
	if (numvisentries < numsides) {
		EraseRect(&itemrect);
		playerh = 10;
		playerbaseline = 8;
		numvisentries = (itemrect.bottom - itemrect.top) / playerh;
		/* (should switch to smaller font also?) */
	}
	tmprgn = NewRgn();
	GetClip(tmprgn);
	ClipRect(&itemrect);
	entryrect = itemrect;
	entryrect.bottom = entryrect.top + playerh;
	InsetRect(&entryrect, 1, 1);
	for (i = 0; i < g_sides_max(); ++i) {
		if (i >= numvisentries) {
			/* Not visible, don't draw. */
		} else if (i < numsides) {
			PenNormal();
			/* Clear this entry. */
			tmprect = entryrect;
			InsetRect(&tmprect, -1, -1);
			EraseRect(&tmprect);
			/* Describe the side. */
			side = assignments[i].side;
			draw_side_emblem(playersetupwin, entryrect.left + 2, entryrect.top + 2,
							 16, 16, side_number(side), shadow_emblem);
			sidetitle = short_side_title(side);
			if (empty_string(sidetitle))
			  sidetitle = "(unnamed)";
			MoveTo(entryrect.left + 2 + 16 + 2, entryrect.top + playerbaseline);
			TextFont(newYork);
			TextFace(0);
			DrawText(sidetitle, 0, strlen(sidetitle));
			/* Only draw other info for sides "in" this game. */
			if (side->ingame) {
				/* Describe the intended player for the side. */
				player = assignments[i].player;
				long_player_title(playertitle, player, "mac");
				MoveTo(entryrect.left + 2 + 130, entryrect.top + playerbaseline);
				TextFace(italic);
				DrawText(playertitle, 0, strlen(playertitle));
				/* Indicate the player's initial advantage. */
				sprintf(spbuf, "%d", player->advantage);
				MoveTo(entryrect.left + 2 + 260, entryrect.top + playerbaseline);
				TextFace(0);
				DrawText(spbuf, 0, strlen(spbuf));
				if (player->password) {
					MoveTo(entryrect.right - 15, entryrect.top + playerbaseline);
					DrawText((strcmp(player->password, "x") == 0 ? "r" : "R"), 0, 1);
				}
			}
			/* (should be able to draw/update selection separately) */
			if (i == selectedplayer) {
				tmprect = entryrect;
				InsetRect(&tmprect, -1, -1);
				InvertRect(&tmprect);
				InsetRect(&tmprect, 3, 3);
				InvertRect(&tmprect);
			} else {
				/* Sides may be present, but not in the game; gray them out. */
				if (!side->ingame)
				  gray_out_rect(&entryrect);
				FrameRect(&entryrect);
			}
		} else {
			/* Draw the available positions as gray outlines. */
			PenPat(QDPat(gray));
			FrameRect(&entryrect);
		}
		OffsetRect(&entryrect, 0, playerh);
	}
	PenNormal();
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
}

/* Callback to draw the little up/down arrow. */

static pascal void
draw_player_setup_advantage(WindowPtr win, short ditem)
{
	int advantage, halfhgt;
	Side *side;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	EraseRect(&itemrect);
	if (updownpicture != nil) {
		DrawPicture(updownpicture, &itemrect);
		/* Never gray out the border, just the arrows inside. */
		InsetRect(&itemrect, 1, 1);
		if (!selected_a_player()) {
			gray_out_rect(&itemrect);
		} else {
			advantage = selected_player()->advantage;
			side = selected_side();
			halfhgt = (itemrect.bottom - itemrect.top) / 2;
			if (advantage <= side->minadvantage) {
				/* Gray out the down arrow. */
				itemrect.top += halfhgt;
				gray_out_rect(&itemrect);
				itemrect.top -= halfhgt;
			}
			if (side->maxadvantage > 0 && advantage >= side->maxadvantage) {
				/* Gray out the up arrow. */
				itemrect.bottom -= halfhgt;
				gray_out_rect(&itemrect);
				itemrect.bottom += halfhgt;
			}
		}
	}
}

static int
adjust_advantage(Player *player, Side *side, int amt)
{
	int newadvantage;

	if (player == NULL)
	  return FALSE;
	newadvantage = player->advantage + amt;
	if (amt > 0 && side->maxadvantage > 0 && newadvantage > side->maxadvantage) {
		beep();
		return FALSE;
	}
	if (amt < 0 && newadvantage < side->minadvantage) {
		beep();
		return FALSE;
	}
	if (newadvantage != player->advantage) {
		player->advantage = newadvantage;
		return TRUE;
	} else
	  return FALSE;
}

void
select_player(int n)
{
	if ((!between(0, n, numsides-1))
	    || (assignments[n].side && !(assignments[n].side->ingame))) {
	    n = -1;
	}
	selectedplayer = n;
	/* (update ditem directly?) */
}

/* Bring up a dialog that allows rearrangement of the side/player assignments.
   This is a movable modal because we might have to be here a while waiting
   for remote players to join in. */

int
player_setup_dialog()
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (draw_player_setup_list_proc == NULL)
	  draw_player_setup_list_proc = NewUserItemProc(draw_player_setup_list);
	if (draw_player_setup_advantage_proc == NULL)
	  draw_player_setup_advantage_proc = NewUserItemProc(draw_player_setup_advantage);
	/* Create the window. */
	if (playersetupwin == nil) {
		playersetupwin = GetNewDialog(dPlayerSetup, NULL, (DialogPtr) -1L);
		GetDItem(playersetupwin, diPlayerSetupList, &itemtype, &itemhandle, &itemrect);
		SetDItem(playersetupwin, diPlayerSetupList, itemtype, (Handle) draw_player_setup_list_proc, &itemrect);
		GetDItem(playersetupwin, diPlayerSetupAdvantage, &itemtype, &itemhandle, &itemrect);
		SetDItem(playersetupwin, diPlayerSetupAdvantage, itemtype, (Handle) draw_player_setup_advantage_proc, &itemrect);
		updownpicture = (PicHandle) GetResource('PICT', pUpDownPicture);
	}
	playerh = 22;
	playerbaseline = 14;
	/* Always start out with the first player selected. */
	selectedplayer = 0;
	set_player_setup_button_states();
	draw_default_button(playersetupwin, diPlayerSetupOK);
	ShowWindow(playersetupwin);
	return TRUE;
}

int numremotewaiting;

/* Set the state of the various buttons and controls.  In general, a side/player
   pair must be selected before anything can be done. */	

void
set_player_setup_button_states()
{
	int locked = g_player_sides_locked();
	int player = (selected_a_player() && !locked);
	int i, numingame = 0;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	numremotewaiting = 0;
	for (i = 0; i < numsides; ++i) {
		if (assignments[i].side && (assignments[i].side)->ingame) {
			++numingame;
			if ((assignments[i].player)->password) {
				if (strcmp((assignments[i].player)->password, "x") == 0)
				  ++numremotewaiting;
			}
		}
	}

#define set_player_button(di, val)  \
	GetDItem(playersetupwin, (di), &itemtype, &itemhandle, &itemrect);  \
	SetDItem(playersetupwin, (di), ((itemtype & 127) | ((val) ? 0 : 128)), itemhandle, &itemrect);  \
	HiliteControl((ControlHandle) itemhandle, ((val) ? 0 : 255));

	set_player_button(diPlayerSetupOK, numremotewaiting == 0);
	set_player_button(diPlayerSetupAdd, !locked && (numsides < g_sides_max()));
	set_player_button(diPlayerSetupRemove, player && !locked && (numsides > g_sides_min()));
	set_player_button(diPlayerSetupRename, player && !(selected_side()->nameslocked));
	set_player_button(diPlayerSetupAI, player);
	set_player_button(diPlayerSetupRemote, player);
	set_player_button(diPlayerSetupExchange, player && numingame > 1);
}

/* Handle any event that we don't want DialogSelect to get. */

void
handle_player_setup_event(EventRecord *event)
{
	char ch;
	Point mouse;

	switch (event->what) {
		case keyDown:
		case autoKey:
			ch = event->message & charCodeMask;
			/* Check for menukey equivalents. */
			if (event->modifiers & cmdKey) {
				if (event->what == keyDown) {
					if (ch == '.') {
						/* (should) Cancel out of player setup. */
						beep();
					}
				}
			} else {
				if (event->what == keyDown) {
					if (ch == 3 || ch == 13) {
						/* The mouse position doesn't actually get used here. */
						mouse.h = mouse.v = 0;
						if (hit_player_setup_dialog(diPlayerSetupOK, mouse))
		  				  launch_game_2();
					}
					/* Shortcuts to tweak the advantage. */
					else if (ch == '+') {
						adjust_advantage(selected_player(), selected_side(), 1);
					} else if (ch == '-') {
						adjust_advantage(selected_player(), selected_side(), -1);
					}
				}
			}
			break;
		case kHighLevelEvent:
			/* Hear from anyone attempting to join via AppleEvent. */
			AEProcessAppleEvent(event);
			break;
	}
}

int
hit_player_setup_dialog(int ditem, Point pt)
{
	int playersok = FALSE, done = FALSE, changedselected = FALSE, changedall = FALSE;
	int willhost = FALSE;
	Player *tmpplayer;
	Side *tmpside;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	tmpplayer = selected_player();
	tmpside = selected_side();

	switch (ditem) {
		case diPlayerSetupOK:
			playersok = TRUE;
			/* Fall through. */
		case diPlayerSetupCancel:
			done = TRUE;
			break;
		case diPlayerSetupAdd:
			add_side_and_player();
			/* Select the newly-created side/player pair. */
			select_player(numsides - 1);
			init_emblem_images(selected_side());
			changedall = TRUE;
			break;
		case diPlayerSetupRemove:
			if (remove_side_and_player()) {
				/* (should fix up) */
			} else {
				beep();
			}
			break;
		case diPlayerSetupRename:
			tmpside->name = NULL;
			tmpside->noun = NULL;
			/* always need to clear this cache before renaming... */
			tmpside->pluralnoun = NULL;
			tmpside->adjective = NULL;
			make_up_side_name(tmpside);
			init_emblem_images(tmpside);
			changedselected = TRUE;
			break;
		case diPlayerSetupAI:
			/* Click between mplayer and not, for now */
			if (tmpplayer->aitypename) {
				tmpplayer->aitypename = NULL;
			} else {
				tmpplayer->aitypename = "mplayer";
			}
			changedselected = TRUE;
			break;
		case diPlayerSetupRemote:
			/* Toggle whether to wait for human to connect in */
			/* (should not use password as flag...) */
			if (tmpplayer->password) {
				tmpplayer->password = NULL;
			} else {
				tmpplayer->password = "x";
				willhost = TRUE;
			}
			changedselected = TRUE;
			break;
		case diPlayerSetupExchange:
			/* Make the next player in the exchange be the selected one,
			   then multiple clicks on the exchange button will move the
			   selected player down through the list. */
			selectedplayer = exchange_players(selectedplayer, -1);
			changedall = TRUE;
			break;
		case diPlayerSetupList:
			GetDItem(playersetupwin, diPlayerSetupList, &itemtype, &itemhandle, &itemrect);
			select_player((pt.v - itemrect.top) / playerh);
			if (tmpplayer != selected_player())
			  changedselected = TRUE;
			break;
		case diPlayerSetupAdvantage:
			GetDItem(playersetupwin, diPlayerSetupAdvantage, &itemtype, &itemhandle, &itemrect);
			if (tmpplayer != NULL) {
				if (pt.v - itemrect.top < ((itemrect.bottom - itemrect.top) / 2)) {
					changedselected = adjust_advantage(tmpplayer, tmpside, 1);
				} else {
					changedselected = adjust_advantage(tmpplayer, tmpside, -1);
				}
			}
			break;
		default:
			break;
	}
	if (!hosting && willhost) {
		connection_method_dialog();
		if (connection_method > 0) {
			hosting = TRUE;
		}
	}
	if (changedall || changedselected) {
		set_player_setup_button_states();
		/* (should minimize redrawing) */
		DrawDialog(playersetupwin);
		draw_default_button(playersetupwin, diPlayerSetupOK);
	}
	if (done) {
		/* Get rid of this dialog, unlikely to need it again. */
		DisposDialog(playersetupwin);
		playersetupwin = nil;
	}
	return playersok;
}

void download_to_player(Player *player);

void
add_remote_player(char *name)
{
	int i;
	Player *player;

	if (name == NULL)
	  name = "somebody";
	for (i = 0; i < numsides; ++i) {
		if (assignments[i].side && (assignments[i].side)->ingame) {
			player = assignments[i].player;
			if (player->password && strcmp(player->password, "x") == 0) {
				player->password = name;
				download_to_player(player);
				set_player_setup_button_states();
				return;
			}
		}
	}
	/* No open positions available (should alert?) */
	beep();
}

void send_assignment PARAMS ((int id, Side *side, Player *player));

void
send_assignment(id, side, player)
int id;
Side *side;
Player *player;
{
    int remoteid;

	low_send(id, "assign");
	low_receive(&remoteid, spbuf, BUFSIZE, 0);
}

/* (should be a generic routine?) */
void
download_to_player(Player *player)
{
	int i, remoteid;

	/* Have the remote program load the game. */
	low_send(player->id, "load");
	low_receive(&remoteid, spbuf, BUFSIZE, 0);
	/* Send it the variant choices. */
	low_send(player->id, "variants");
	low_receive(&remoteid, spbuf, BUFSIZE, 0);
	/* Send it the current list of side/player assignments. */
	for (i = 0; i < numsides; ++i) {
		send_assignment(player->id, assignments[i].side, assignments[i].player);
	}
	++numremotes;
}

/* This dialog opens a player-specified file as a game. */

int
open_game_dialog()
{
	Point pnt;
	SFTypeList typelist;
	SFReply reply;

	/* Gotta be somewhere... */
	SetPt(&pnt, 100, 100);
	/* Game module are text files. */
	typelist[0] = 'TEXT';
	SFGetFile(pnt, "\p", NULL, 1, typelist, NULL, &reply);
	if (reply.good) {
		return open_game_from_name_and_volume(reply.fName, reply.vRefNum);
	}
	return FALSE;
}

/* Open a game file using an FSSpec, which will typically come from an AppleEvent. */

int
open_game_from_fsspec(FSSpec *fsspec)
{
	short vrefnum;

	/* (should check the return result) */
	OpenWD(fsspec->vRefNum, fsspec->parID, XconqSignature, &vrefnum);
	return open_game_from_name_and_volume(fsspec->name, vrefnum);
}

/* Given a filename and volume number, load, check, and launch. */

int
open_game_from_name_and_volume(Str255 name, int vrefnum)
{
	char modulename[256];
	Module *module;

	/* Set the current volume to be where the file is, so path-less
	   fopen() in module loading below will find the file.  Note that this
	   won't work for loading multiple modules from multiple non-library
	   locations, but this dialog can only load one file at a time anyway. */
	SetVol(nil, vrefnum);
	clear_game_modules();
	/* Build a module with the given filename. */
	module = get_game_module(NULL);
	p2c(name, modulename);
	module->filename = copy_string(modulename);
	module->hook = NULL;
	/* This module is effectively the "selected game", whether or not
	   it's loadable. */
	selected_game = module;
	/* Load the module. */
	mainmodule = module;
	load_game_module(module, TRUE);
	/* Change cursor back, in case it was different during loading. */
	SetCursor(&QD(arrow));
	if (!module->loaded) {
		init_warning("Could not load a module from %s", module->filename);
		return FALSE;
	}
	/* If the loaded module has bugs, we will get alerts here. */
	check_game_validity();
	/* We can now join up with the "new game" startup sequence. */
	return launch_game();
}

/* This is just the collection of init steps shared by both new and open dialogs.
   If the player doesn't cancel out of anything, then the game will be running when
   this routine returns. */

int
launch_game()
{
	/* Bring up the pre-player-setup variants. */
	if (!variants_dialog()) {
		/* Cancelled from variants dialog, go back to wherever. */
		return FALSE;
	}
	/* Load any colors first; we can use them to substitute for patterns. */
	init_terrain_colors();
	/* Set up the images we'll be needing. */
	init_terrain_images();
	init_unit_images();
	/* Trial side/player assignment, will create the default player if nececessary. */
	make_trial_assignments();
	/* Get the emblems so they can be displayed in the player dialog. */
	init_all_emblem_images();
	/* Ask for modifications to the trial assignment. */
	return player_setup_dialog();
}

void
launch_game_2()
{
	/* Complain if any images were missing.  Will also note if image resource files
	   were never found - might be one of the explanations. */
	check_for_missing_images();
	/* A last-minute patch to ensure reasonable area dimensions. */
	if (area.width == 0 || area.height == 0)
	  set_area_shape(DEFAULTWIDTH, DEFAULTHEIGHT, TRUE);
	/* About to do lengthy init calcs, show the progress bar. */
	open_progress_dialog();
	/* Synthesize the remainder of the initial game setup. */
	calculate_globals();
	run_synth_methods();
	final_init();
	assign_players_to_sides();
	/* No more interminable init calcs (we hope), so close the progress bar. */
	close_progress_dialog();
	/* Init the display now. */
	init_display();
	/* Do first set of turn calcs. */
	run_game(0);
}

/* This counts up all the images that had to be manufactured, and maybe puts up an
   alert asking if the player wants to go with the substitutes. */

void
check_for_missing_images()
{
	int u, t, e;
	Side *side;
	Str255 tmpstr;

	for_all_terrain_types(t) {
		if (timages[t]->ersatz && !t_color_defined(t))
		  record_missing_image(TTYP, t_type_name(t));
	}
	for_all_unit_types(u) {
		if (uimages[u]->ersatz)
		  record_missing_image(UTYP, u_type_name(u));
	}
	for_all_sides(side) {
		e = side_number(side);
		if (eimages[e]->ersatz)
		  record_missing_image(3, side_desig(side)); 
	}
	if (missing_images(tmpbuf)) {
		c2p(tmpbuf, tmpstr);
		ParamText(tmpstr, (foundimagesfile ? "\p" : "\p & no image resource files found"), "\p", "\p");
		switch (CautionAlert(aImagesMissing, nil)) {
			case 1:
				/* We decided to live with low-quality images. */
				break;
			case 2:
				/* Ugliness is unbearable, get out now. */
				ExitToShell();
				break;
		}
	}
}

/* Rotate around "marching parentheses" cursors to indicate reading is happening. */

void
announce_read_progress()
{
	if (readprogressors[curcurs] != nil)
	  SetCursor(*(readprogressors[curcurs]));
	curcurs = (curcurs + 1) % NUMcParens;
}

/* Display progress in lengthy initialization processes. */

/* This is the app-defined item that actually draws the progress bar. */

static pascal void
draw_progress(WindowPtr win, short ditem)
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	GetDItem(win, ditem, &itemtype, &itemhandle, &itemrect);
	if (progress >= 0) {
		/* Only draw the gray bg initially or when "progress" decreases (would be a
		   bug in synth method, but useful to get visual evidence if it happens). */
		if (lastprogress < 0 || lastprogress > progress) {
			FillRect(&itemrect, QDPat(gray));
			FrameRect(&itemrect);
		}
		itemrect.right =
			itemrect.left + (progress * (itemrect.right - itemrect.left)) / 100;
		FillRect(&itemrect, QDPat(black));
	} else {
		EraseRect(&itemrect);
	}
}

/* This starts off the lengthy process. */

void
open_progress_dialog()
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (draw_progress_proc == NULL)
	  draw_progress_proc = NewUserItemProc(draw_progress);
	progresswin = GetNewDialog(dProgress, NULL, (DialogPtr) -1L);
	GetDItem(progresswin, diProgressBar, &itemtype, &itemhandle, &itemrect);
	SetDItem(progresswin, diProgressBar, itemtype, (Handle) draw_progress_proc, &itemrect);
	curcurs = 0;
	if (progressors[curcurs] != nil)
	  SetCursor(*(progressors[curcurs]));
}

/* This is called to announce the beginning of a lengthy process. */

void
announce_lengthy_process(char *msg)
{
	Str255 tmpstr;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (progresswin == nil)
	  return;
	/* Paste in the description of what is going to take so long. */
	GetDItem(progresswin, diProgressText, &itemtype, &itemhandle, &itemrect);
	c2p(msg, tmpstr);
	SetIText(itemhandle, tmpstr);
	/* Initialize the progress bar itself. */
	progress = -1;
	lastprogress = 0;
	ShowWindow(progresswin);
	SelectWindow(progresswin);
	Dprintf("%s;", msg);
	curcurs = 0;
	if (progressors[curcurs] != nil)
	  SetCursor(*(progressors[curcurs]));
}

/* This is called periodically to say how much progress has been made. */

void
announce_progress(int pctdone)
{
	if (progresswin == nil)
	  return;
	/* (should notice that stop button was hit and use to escape from program) */
	SelectWindow(progresswin);
	lastprogress = progress;
	progress = min(max(0, pctdone), 100);
	/* Redraw the progress bar app-defined item, but only if there was
	   any change to report. */
	/* (should restrict to bar alone, otherwise flickers a lot) */
	if (progress != lastprogress)
	  DrawDialog(progresswin);
	/* Switch to the next cursor in the animation sequence. */
	if (progressors[curcurs] != nil)
	  SetCursor(*(progressors[curcurs]));
	curcurs = (curcurs + 1) % NUMcSynth;
	Dprintf(" %d%%", pctdone);
}

/* This is called to signal the end of the lengthy process. */

void
finish_lengthy_process()
{
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (progresswin == nil)
	  return;
	SelectWindow(progresswin);
	/* Replace the text with a "reassuring" but uninformative message. */
	GetDItem(progresswin, diProgressText, &itemtype, &itemhandle, &itemrect);
	SetIText(itemhandle, "\pKeeping busy...");
	progress = -1;
	/* Force a redraw, which will cause the progress bar to be erased. */
	DrawDialog(progresswin);
	Dprintf(" done.\n");
	curcurs = 0;
	if (progressors[curcurs] != nil)
	  SetCursor(*(progressors[curcurs]));
}

/* When we're completely done with all lengthy init processes, discard the dialog. */

void
close_progress_dialog()
{
	/* Restore the normal cursor. */
	SetCursor(&QD(arrow));
	DisposDialog(progresswin);
	progresswin = nil;
}

/* Kernel callback to put in a default player, assumed to be on
   "this" screen (which we call "mac", for lack of inspiration). */

Player *
add_default_player()
{
	Player *player = add_player();
	
	player->displayname = "mac";
	return player;
}

/* This is a kernel callback that sets the given side to have a user
   interface.  On the Mac, we just record the side as `dside', will
   finish opening later. */

void
init_ui(Side *side)
{
	if (side != NULL) {
		side->ui = (UI *) xmalloc(sizeof(UI));
		side->ui->active = FALSE;
		dside = side;
		/* Make sure this is allocated.  Note that xmalloc clears, so this
		   will look like it has an empty Pascal string. */
		if (curdatestr == NULL)
		  curdatestr = (char *) xmalloc(100);
		interp_mac_ui_data(find_at_key(dside->uidata, "mac"));
	}
}

/* Flag the screen as officially "open", init random globals, and open the
   initial set of windows. */

void
init_display()
{
	int p;
	Handle handle;

	/* If dside never got assigned, try some hasty repairs. */
	if (dside == NULL) {
		init_warning("Nobody wanted a display!");
		/* Make an arbitrary choice - pick the first side. */
		init_ui(side_n(1));
		if (dside == NULL) {
			/* Geez, nothing is going right. */
			init_error("Can't set up the display!");
		}
	}
	if (bordbitmaps == NULL) {
		bordbitmaps = (BitMap *) xmalloc(NUMPOWERS * sizeof(BitMap));
	}
	if (connbitmaps == NULL) {
		connbitmaps = (BitMap *) xmalloc(NUMPOWERS * sizeof(BitMap));
	}
	/* Init polygon-caching machinery. */
	for (p = 0; p < NUMPOWERS; ++p) {
		polygons[p] = nil;
		lastpolyx[p] = lastpolyy[p] = 0;
		cellrgns[p] = gridcellrgns[p] = nil;
		lastcellrgnx[p] = lastcellrgny[p] = 0;
		lastgridcellrgnx[p] = lastgridcellrgny[p] = 0;
		cellrgns30[p] = gridcellrgns30[p] = nil;
		lastcellrgnx30[p] = lastcellrgny30[p] = 0;
		lastgridcellrgnx30[p] = lastgridcellrgny30[p] = 0;
		cellrgns15[p] = gridcellrgns15[p] = nil;
		lastcellrgnx15[p] = lastcellrgny15[p] = 0;
		lastgridcellrgnx15[p] = lastgridcellrgny15[p] = 0;
		bordbitmaps[p].rowBytes = 0;
		connbitmaps[p].rowBytes = 0; 
	}
	handle = GetResource('ICN#', 140);
	if (handle != nil) {
		HLock(handle);
		bordbitmaps[4].baseAddr = *handle;
		bordbitmaps[4].rowBytes = 4;
		SetRect(&(bordbitmaps[4].bounds), 0, 0, 32, 32);
	}
	handle = GetResource('ICN#', 141);
	if (handle != nil) {
		HLock(handle);
		connbitmaps[4].baseAddr = *handle;
		connbitmaps[4].rowBytes = 4;
		SetRect(&(connbitmaps[4].bounds), 0, 0, 32, 32);
	}
	/* Init assorted list heads and window pointers. */
	maplist = NULL;
	listlist = NULL;
	unitcloseuplist = NULL;
	historywin = nil;
	init_ui_chars();
	/* Flag the interface as ready to display stuff.  After this point,
	   any events causing display activity will actually be rendered
	   on the screen. */
	dside->ui->active = TRUE;
	/* Now bring up the initial collection of windows. */
	if (0 /* should restore preferred windows */) {
	} else {
		/* Indicate that this is the first time these windows are being
		   shown; (should) adjust to fit the screen size. */
		first_windows = TRUE;
		/* The game window is always useful. */
		create_game_window();
		ShowWindow(gamewin);
		/* The notice window is always useful. */
		create_notice_window();
		ShowWindow(noticewin);
		/* This will be the main playing display, so it should be up. */
		create_map(5);
		/* This needs to come up so that players unfamiliar with the game
		   will have some guidance. */
		instructions_dialog();
		first_windows = FALSE;
	}
#ifdef DESIGNERS
	/* If we're designing, bring up the palette automatically. */
	if (dside->designer) {
		create_design_window();
	}
#endif /* DESIGNERS */
	Dprintf("Opened Mac display!\n");
}

/* Allocate and fill in images for all the unit types. */

void
init_unit_images()
{
	int u;

	uimages = (ImageFamily **) xmalloc(numutypes * sizeof(ImageFamily *));

	for_all_unit_types(u) {
		uimages[u] = get_unit_type_images(dside, u, mac_interp_imf, mac_load_imf,
										  add_default_unit_image);
	}
}

/* The default unit image is a box with the name of the unit inside. */

static void
add_default_unit_image(ImageFamily *imf, int u, char *name)
{
	Rect tmprect;
	PicHandle pichandle;
	RgnHandle tmprgn;
	Image *img;
	MacImage *macimg;

	img = get_img(imf, 32, 32);
    if (img == NULL)
      return;
	img->istile = FALSE;
	macimg = get_mac_image(img);
	tmprgn = NewRgn();
	GetClip(tmprgn);
	SetRect(&tmprect, 0, 0, 32, 32);
	ClipRect(&tmprect);
	pichandle = OpenPicture(&tmprect);
	InsetRect(&tmprect, 2, 2);
	FillRect(&tmprect, QDPat(white));
	InsetRect(&tmprect, 1, 1);
	FrameRect(&tmprect);
	MoveTo(4, 20);
	DrawText(name, 0, strlen(name));
	ClosePicture();
	SetClip(tmprgn);
	DisposeRgn(tmprgn);
	macimg->monopict = pichandle;
}

/* Allocate space and load a named color for each terrain type
   that defines one. */

void
init_terrain_colors()
{
	int t;

	if (hasColorQD) {
		tcolors = (ImageColor **) xmalloc(numttypes * sizeof(ImageColor *));
		for_all_terrain_types(t) {
			if (!empty_string(t_color(t))) {
				tcolors[t] = new_image_color(t_color(t));
				mac_load_image_color(tcolors[t]);
			}
		}
	}
	interp_named_color(g_grid_color(), &gridgray, &gridcolor);
	interp_named_color(g_unseen_color(), &unseengray, &unseencolor);
	/* If the grid color and the unseen terrain color are the same, then we can
	   draw more efficiently. */
	if (strcmp(g_grid_color(), g_unseen_color()) == 0
	    && empty_string(g_unseen_image_name()))
	  grid_matches_unseen = TRUE;
	blackcolor.red = blackcolor.green = blackcolor.blue = 0;
}

/* Set up the collection of terrain images/patterns, making a default pattern
   if necessary. */

void
init_terrain_images()
{
	int t;

	timages = (ImageFamily **) xmalloc(numttypes * sizeof(ImageFamily *));

	for_all_terrain_types(t) {
		timages[t] = get_terrain_type_images(dside, t, mac_interp_imf, mac_load_imf,
											 add_default_terrain_image);
	}
	get_unseen_images(dside, mac_interp_imf, mac_load_imf, NULL);
}

/* Synthesize a terrain pattern, using the type to make a distinguishing
   pattern. Basically the bits of the type number make a sort of bar code,
   but enclosed in a box and with a horizontal bar through the middle, so
   as to make it easier to tell which is which on a map.  (Trust me, I've
   already experimented with this.) */

static void
add_default_terrain_image(ImageFamily *imf, int t, char *name)
{
	Image *img;
	MacImage *macimg;

	img = get_img(imf, 8, 8);
	if (img == NULL)
	  return;
	img->minw = img->minh = 1;
	img->maxw = img->maxh = 9999;
	img->istile = TRUE;
	macimg = get_mac_image(img);
	SET_IMG_PAT (macimg, 0, 0x01);
	SET_IMG_PAT (macimg, 1, (t << 2) + 0x01);
	SET_IMG_PAT (macimg, 2, (t << 2) + 0x01);
	SET_IMG_PAT (macimg, 3, 0x7d);
	SET_IMG_PAT (macimg, 4, (t << 2) + 0x01);
	SET_IMG_PAT (macimg, 5, (t << 2) + 0x01);
	SET_IMG_PAT (macimg, 6, 0x01);
	SET_IMG_PAT (macimg, 7, 0xff);
	macimg->patdefined = 1;
	/* Count this type as having only a default pattern, but only if we can't
	   possibly render the terrain with a solid color. */
	if (!(hasColorQD && t_color_defined(t)))
	  imf->ersatz = TRUE;
}

/* Given a color name, get actual RGB values for it.  As a special case for b/w
   screens, the five shades of gray also get set here. */

void
interp_named_color(char *name, enum grays *grayvar, RGBColor *colorvar)
{
	ImageColor tmpcolor;

	if (strcmp(name, "black") == 0) {
		*grayvar = blackgray;
		colorvar->red = colorvar->green = colorvar->blue = 0;
	} else if (strcmp(name, "dark gray") == 0) {
		*grayvar = darkgray;
		colorvar->red = colorvar->green = colorvar->blue = 16384;
	} else if (strcmp(name, "gray") == 0) {
		*grayvar = mediumgray;
		colorvar->red = colorvar->green = colorvar->blue = 32768;
	} else if (strcmp(name, "light gray") == 0) {
		*grayvar = lightgray;
		colorvar->red = colorvar->green = colorvar->blue = 49000;
	} else if (strcmp(name, "white") == 0) {
		*grayvar = whitegray;
		colorvar->red = colorvar->green = colorvar->blue = 65535;
	} else if (hasColorQD) {
		tmpcolor.name = name;
		mac_load_image_color(&tmpcolor);
		colorvar->red = tmpcolor.r;
		colorvar->green = tmpcolor.g;
		colorvar->blue = tmpcolor.b;
	}
}

/* Load/create all the images of each side's emblem. */

void
init_all_emblem_images()
{
	Side *side2;

	eimages = (ImageFamily **) xmalloc((g_sides_max() + 1) * sizeof(ImageFamily *));

	for_all_sides_plus_indep(side2) {
		init_emblem_images(side2);
	}
}

/* Set up the emblems for a particular side.  This is used both to do all sides
   initially and whenever a side changes emblems. */

void
init_emblem_images(Side *side2)
{
	int e;

	e = side_number(side2);
	eimages[e] = get_emblem_images(dside, side2, mac_interp_imf, mac_load_imf,
								   add_default_emblem_image);
}

/* Default image is SICN-sized number.  Use only if no images have been found
   and the emblem name is not "none". */

static void
add_default_emblem_image(ImageFamily *imf, int e, char *name)
{
	int i;
	char tmpbuf[10];
	Str255 tmpstr;
	Handle sicnhandle, maskhandle;
	Rect tmprect;
	PicHandle pichandle;
	RgnHandle tmprgn;
	Image *img;
	MacImage *macimg;

	img = get_img(imf, 16, 16);
	img->minw = img->minh = 8;
	img->maxw = img->maxh = 64;
	/* Emblems never tile. */
	img->istile = FALSE;
	macimg = get_mac_image(img);
	sprintf(tmpbuf, "s%d", e);
	c2p(name, tmpstr);
	sicnhandle = GetNamedResource('SICN', tmpstr);
	if (tmpstr != nil) {
		/* Found a sicn with the side's number in it. */
		/* Image itself is the first 32 bytes, mask is second 32 if present. */
		macimg->monosicn = sicnhandle;
		maskhandle = (Handle) NewHandle(32);
		if (SizeResource(sicnhandle) >= 64) {
			for (i = 0; i < 32; ++i) {
				(*maskhandle)[i] = (*sicnhandle)[i+32];
			}
		} else {
			/* Set up an all-white background. */
			for (i = 0; i < 32; ++i) {
				(*maskhandle)[i] = 0xff;
			}
		}
		macimg->masksicn = maskhandle;
	} else {
		/* If we couldn't even find a number sicn, build a crude substitute pict. */
		imf->ersatz = TRUE;
		tmprgn = NewRgn();
		GetClip(tmprgn);
		SetRect(&tmprect, 0, 0, 16, 16);
		ClipRect(&tmprect);
		pichandle = OpenPicture(&tmprect);
		FillRect(&tmprect, QDPat(white));
		MoveTo(2, 12);
		DrawText(tmpbuf+1, 0, strlen(tmpbuf+1));
		ClosePicture();
		SetClip(tmprgn);
		DisposeRgn(tmprgn);
		macimg->monopict = pichandle;
	}
}
