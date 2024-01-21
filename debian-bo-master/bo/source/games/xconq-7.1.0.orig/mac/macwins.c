/* Handling of assorted minor windows for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern int construction_possible PARAMS ((int u2));
extern int can_build_or_help PARAMS ((Unit *unit));
extern char *side_score_desc PARAMS ((char *buf, Side *side, Scorekeeper *sk));
#include "macconq.h"
extern void set_construction_run_length(int len);

#define DEFAULT_RUN (99)

static void adjust_construction_items(void);

static pascal void history_scroll_fn(ControlHandle control, short code);
static pascal void notice_vscroll_fn(ControlHandle control, short code);
static pascal void scores_vscroll_fn(ControlHandle control, short code);

/* Globals for the game window. */

WindowPtr gamewin = nil;

int gamewinw = 200;
int gamedatehgt = 18;
int gameclockoffset;
int gameclockhgt = 15;
int gamenoteoffset;
int gamenotehgt = 15;
int gametophgt;

int gamesidehgt = 24;
int gamesideclockhgt = 15;
int gamesideclockoffset;
int gamesidescorehgt = 15;
int gamesidescoreoffset;

int gamenumsides;

Handle aisicnhandle = nil;
Handle facesicnhandle[3];

char *game_progress_str = "";

time_t lastnow;

/* Globals for the construction window. */

WindowPtr constructionwin = nil;

static ListHandle construction_unit_list = nil;
static ListHandle construction_type_list = nil;

static int constructmargin = 5;
static int constructtop = 32;

/* This is the vector of units that can do construction or research. */

static UnitVector *constructor_units = NULL;

static int numposstypes;
static int *possibletypes = NULL;

int currunlength;

int editedrunlength = -1;

/* (this is referenced from the end-turn command, so can't make static) */
ControlHandle constructbutton;
static ControlHandle researchbutton;

TEHandle run_length_text = nil;

static Rect runlengthrect;
static Rect unitlistrect;
static Rect typelistrect;

/* Globals for unit closeup windows. */

int lastunitcloseuph = -1, lastunitcloseupv = -1;

static int m_per_row = 2;

static char *nummrows;

int closeupspacing = 16;

int closeupwinwid = 200;

/* Globals for the history window. */

WindowPtr historywin = nil;

HistEvent **histcontents = NULL;

int numhistcontents = 0;

ControlHandle histvscrollbar;

int history_line_spacing = 15; /* (should derive from font) */

int history_top_line_height = 25;

HistEvent *firstvisevt = NULL;

HistEvent *secondvisevt = NULL;

int numvishistlines;

int total_history_lines = 0;

ControlActionUPP history_scroll_proc;

/* Globals for the notice window. */

DialogPtr noticewin = nil;

static TEHandle notice_text = nil;

static ControlHandle notice_v_scrollbar;

ControlActionUPP notice_vscroll_proc;

/* Globals for the scores window. */

DialogPtr scoreswin = nil;

static TEHandle scores_text = nil;

static ControlHandle scores_v_scrollbar;

ControlActionUPP scores_vscroll_proc;

/* The game progress window. */

/* Create the game progress window. */

void
create_game_window()
{
	int screenwidth;
	extern int numscores;

	/* Create the window, color if possible, since emblems may be in color. */
	if (hasColorQD) {	
		gamewin = GetNewCWindow(wGame, NULL, (WindowPtr) -1L);
	} else {
		gamewin = GetNewWindow(wGame, NULL, (WindowPtr) -1L);
	}
	gametophgt = gamedatehgt;
	if (g_rt_per_turn() > 0 || g_rt_for_game() > 0) {
		gameclockoffset = gametophgt;
		gametophgt += gameclockhgt;
	}
	gamenoteoffset = gametophgt;
	gametophgt += gamenotehgt;
	/* Add some space if sides have a per-turn and/or per-game clock. */
	if (g_rt_per_side() > 0) {
		gamesideclockoffset = gamesidehgt;
		gamesidehgt += gamesideclockhgt;
	}
	/* Add additional space for each two scorekeepers. */
	if (keeping_score()) {
		gamesidescoreoffset = gamesidehgt;
		gamesidehgt += gamesidescorehgt * ((numscorekeepers + 1) / 2);
	}
	/* This is not growable, so we have to ensure it's big enough to start with. */
	/* (Record the current numsides so we can grow the window later.) */
	gamenumsides = numsides;
	SizeWindow(gamewin, gamewinw, gametophgt + gamenumsides * gamesidehgt, 1);
	if (first_windows) {
		get_main_screen_size(&screenwidth, NULL);
		MoveWindow(gamewin, screenwidth - gamewinw - 3, 40, FALSE);
	}
	/* Get handles to useful sicns. */
	aisicnhandle = GetNamedResource('SICN', "\pmplayer");
	facesicnhandle[0] = GetNamedResource('SICN', "\phostile");
	facesicnhandle[1] = GetNamedResource('SICN', "\pneutral");
	facesicnhandle[2] = GetNamedResource('SICN', "\pfriendly");
}

char *last_status;
char *last_status_left;
char *last_status_resv;
char *last_status_fini;
char **last_status_score1;

void
draw_game()
{
	Side *side2;
	GrafPtr oldport;

	if (gamewin == nil)
	  return;
	/* The window might need to get bigger. */
	if (numsides != gamenumsides) {
		last_status = NULL;
		last_status_left = NULL;
		last_status_resv = NULL;
		last_status_fini = NULL;
		last_status_score1 = NULL;
		gamenumsides = numsides;
		SizeWindow(gamewin, gamewinw, gametophgt + gamenumsides * gamesidehgt, 1);
	}
	if (last_status == NULL)
	  last_status = xmalloc(numsides + 1);
	if (last_status_left == NULL)
	  last_status_left = xmalloc(numsides + 1);
	if (last_status_resv == NULL)
	  last_status_resv = xmalloc(numsides + 1);
	if (last_status_fini == NULL)
	  last_status_fini = xmalloc(numsides + 1);
	if (last_status_score1 == NULL) {
		int i;
		last_status_score1 = (char **) xmalloc((numsides + 1) * sizeof(char *));
		for (i = 0; i <= numsides; ++i)
		  last_status_score1[i] = xmalloc(100);
	}
	GetPort(&oldport);
	SetPort(gamewin);
	draw_game_date();
	draw_game_progress();
	/* Draw a solid separating line between date info and side list. */
	MoveTo(0, gametophgt);
	Line(gamewinw, 0);
	for_all_sides(side2)
	  draw_game_side(side2);
	SetPort(oldport);
}

/* Display the current time and date and any realtime countdowns. */

void
draw_game_date()
{
	Rect tmprect;

	SetRect(&tmprect, 0, 0, gamewinw, gamedatehgt - 1);
	EraseRect(&tmprect);
	MoveTo(tmprect.left + 10, tmprect.top + 12);
	TextFace(bold);
	DrawString((unsigned char *) curdatestr);
	TextFace(0);
	/* (should draw season name here somewhere?) */
	draw_game_clocks();
	if (endofgame) {
		gray_out_rect(&tmprect);
	}
#ifdef DEBUGGING
    /* Indicate the state of all the debug flags. */
	if (Debug || DebugM || DebugG) {
		sprintf(spbuf, "%c%c%c%c",
				(Debug ? 'D' : ' '), (DebugM ? 'M' : ' '), (DebugG ? 'G' : ' '),
				(Profile ? 'P' : ' '));
		MoveTo(tmprect.right - 30, tmprect.top + 12);
		DrawText(spbuf, 0, strlen(spbuf));
	}
#endif /* DEBUGGING */
}

void
draw_game_clocks()
{
	int elapsed, s2, sy;
    time_t now;
	Rect tmprect;
	Side *side2;

	/* Draw per-turn and per-game time limits that for the game as a whole. */
	SetRect(&tmprect, 0, gamedatehgt, gamewinw / 2, gamedatehgt + gameclockhgt - 1);
	if (g_rt_per_turn() > 0) {
		time(&now);
    	elapsed = (int) difftime(now, turn_play_start_in_real_time);
		time_desc(spbuf, g_rt_per_turn() - elapsed, g_rt_per_turn());
		EraseRect(&tmprect);
		MoveTo(tmprect.left + 20, tmprect.top + 10);
		DrawText(spbuf, 0, strlen(spbuf));
		lastnow = now;
	}
	OffsetRect(&tmprect, 100, 0);
	if (g_rt_for_game() > 0) {
		time(&now);
    	elapsed = (int) difftime(now, game_start_in_real_time);
		time_desc(spbuf, g_rt_for_game() - elapsed, g_rt_for_game());
		EraseRect(&tmprect);
		MoveTo(tmprect.left + 10, tmprect.top + 10);
		DrawText(spbuf, 0, strlen(spbuf));
		lastnow = now;
	}
	/* Draw per-side clocks if any limits defined. */
	if (g_rt_per_side() > 0) {
		for_all_sides(side2) {
			if (side2->ingame) {
				s2 = side_number(side2);
				sy = gametophgt + (s2 - 1) * gamesidehgt + gamesideclockoffset;
				elapsed = 0;
				if (!side2->finishedturn)
    			  elapsed = (int) difftime(now, turn_play_start_in_real_time); /* should be side start */
				time_desc(spbuf, g_rt_per_side() - side2->totaltimeused - elapsed, g_rt_per_side());
				SetRect(&tmprect, 0, sy, gamewinw, sy + gamesideclockhgt - 1);
				EraseRect(&tmprect);
				MoveTo(tmprect.left + 20, tmprect.top + 10);
				DrawText(spbuf, 0, strlen(spbuf));
				/* (should draw per-turn side usage) */
			}
		}
	}
}

void
draw_game_progress()
{
	Rect tmprect;

	SetRect(&tmprect, 0, gamenoteoffset, gamewinw, gamenoteoffset + gamenotehgt - 1);
	EraseRect(&tmprect);
	MoveTo(1, gamenoteoffset + 12);
	DrawText(game_progress_str, 0, strlen(game_progress_str));
}

/* Draw info about a given side. */

void
draw_game_side(Side *side2)
{
	int s2 = side_number(side2);
	int sx = 20, sy = gametophgt + (s2 - 1) * gamesidehgt;

	draw_side_emblem(gamewin, 2, sy + 4, 16, 16, s2, shadow_emblem);
	strcpy(spbuf, short_side_title(side2));
	MoveTo(sx, sy + 12);
	/* Put the name of our side in boldface. */
	TextFace((side2 == dside ? bold : 0));
	DrawText(spbuf, 0, strlen(spbuf));
	TextFace(0);
	if (side_has_ai(side2) && side2->ingame) {
		/* Show that the side is run by an AI. */
		plot_sicn(gamewin, 182, sy + 2, aisicnhandle, 0, TRUE, srcOr);
	}
	if (side2 != dside
	    && side2->ingame
	    && (side_has_ai(side2) || side_has_display(side2))) {
		/* Indicate attitude of other side. */
		plot_sicn(gamewin, 164, sy + 2,
			facesicnhandle[feeling_towards(side2, dside)], 0, TRUE, srcOr);
	}
	last_status[s2] = -1;
	last_status_left[s2] = -1;
	last_status_resv[s2] = -1;
	last_status_fini[s2] = -1;
	*(last_status_score1[s2]) = '\0';
	draw_side_status(side2);
	/* Draw a separating line. */
	PenPat(QDPat(gray));
	MoveTo(0, sy + gamesidehgt);
	Line(gamewinw, 0);
	PenNormal();
}

/* (should make this more generic) */

int
feeling_towards(Side *side, Side *side2)
{
	if (trusted_side(side2, side)) {
		return 2;
	} else if (side_has_ai(side) && should_try_to_win(side)) {
		return 0;
	} else {
		return 1;
	}
}

/* Draw the current details about a side. */

void
draw_side_status(Side *side2)
{
	int s2 = side_number(side2);
	int sx, sy, i;
	int totacp, resvacp, acpleft, percentleft, percentresv;
	int newstatus;
	char *scoredesc;
	Rect siderect, tmprect, progressrect;
    Scorekeeper *sk;
    extern int curpriority;

    /* Be safe.  It's probably not great to be passing through here before the game
       window is actually set up, but it's easier to just ignore the attempt than
       to figure out why it's attempting... */
	if (last_status == NULL)
	  return;
	sy = gametophgt + (s2 - 1) * gamesidehgt;
	newstatus = last_status[s2];
	percentleft = 0;
	SetRect(&siderect, 0, sy + 1, gamewinw, sy + gamesidehgt);
	/* Set up the area where we show progress. */
	SetRect(&progressrect, 20, sy + 12 + 4, 20 + 100, sy + 12 + 4 + 7);
	if (!side2->ingame || endofgame) {
		if (last_status[s2] != 0) {
			gray_out_rect(&siderect);
			if (side_won(side2)) {
				/* (should) Indicate that this side has won. */
				/* draw like a trophy or flourishes or some such?) */
			} else if (side_lost(side2)) {
				/* Draw a (solid) line crossing out the loser.  Simple and obvious. */
				MoveTo(1, sy + 8);
				Line(gamewin->portRect.right - 3, 0);
			}
			newstatus = 0;
		}
	} else {
		if (!g_use_side_priority() || curpriority == side2->priority) {
			/* Show the current acp totals/progress of the side. */
			/* This is not quite the security hole it might seem,
			   you don't get much advantage out of seeing how far along each side is,
			   and it gives you a feel for how the turn is progressing. */
			totacp = side_initacp(side2);
			if (totacp > 0) {
				acpleft = side_acp(side2);
				resvacp = side_acp_reserved(side2);
				if (totacp > 0) {
					percentleft = (100 * acpleft) / totacp;
					percentleft = max(0, min(99, percentleft));
					percentresv = (100 * resvacp) / totacp;
					/* Acp in reserve should be less than acp total. */
					percentresv = max(0, min(percentleft, percentresv));
				} else {
					percentleft = percentresv = 0;
				}
				/* Only draw if there's been any actual change. */
				if (last_status[s2] != 1
					|| last_status_left[s2] != percentleft
					|| last_status_resv[s2] != percentresv) {
					if (percentleft > 0) {
						tmprect = progressrect;
						InsetRect(&tmprect, 1, 1);
						EraseRect(&tmprect);
						tmprect.right = tmprect.left + percentleft;
						FillRect(&tmprect, QDPat(black));
					}
					if (percentresv > 0) {
						tmprect = progressrect;
						InsetRect(&tmprect, 1, 1);
						tmprect.right = tmprect.left + percentresv;
						FillRect(&tmprect, QDPat(dkGray));
					}
					last_status_left[s2] = percentleft;
					last_status_resv[s2] = percentresv;
				}
				newstatus = 1;
			} else {
				if (last_status[s2] != 2) {
					tmprect = progressrect;
					InsetRect(&tmprect, 1, 1);
					EraseRect(&tmprect);
					newstatus = 2;
				}
			}
		} else if (g_use_side_priority() && curpriority != side2->priority) {
			newstatus = 0;
		}
		/* (should this be a generic kernel test?) */
		if (side2->finishedturn || !(side_has_ai(side2) || side_has_display(side2))) {
			if (last_status_fini[s2] != 1) {
				tmprect = progressrect;
				InsetRect(&tmprect, 1, 1);
				EraseRect(&tmprect);
				tmprect.right = tmprect.left + percentleft;
				FillRect(&tmprect, QDPat(gray));
				last_status_fini[s2] = 1;
			}
		} else if (!side2->finishedturn) {
			last_status_fini[s2] = 0;
		}
	}
	/* Decide how to frame the progress bar - each shade indicates something. */
	if (newstatus != last_status[s2]) {
		if (newstatus == 0) {
			EraseRect(&progressrect);
			PenPat(QDPat(white));
		} else if (newstatus == 1) {
			PenPat(QDPat(black));
		} else if (newstatus == 2) {
			PenPat(QDPat(gray));
		} else {
			PenPat(QDPat(ltGray));
		}
		FrameRect(&progressrect);
		PenNormal();
		last_status[s2] = newstatus;
	}
	/* Always draw the score, and always ungrayed. */
	if (keeping_score()) {
		siderect.top += gamesidescoreoffset;
		siderect.bottom = siderect.top + gamesidescorehgt - 1;
		i = 0;
		for_all_scorekeepers(sk) {
			scoredesc = side_score_desc(spbuf, side2, sk);
			if ((i == 0 && strcmp(scoredesc, last_status_score1[s2]) != 0)
				|| i > 0) {
				if ((i & 1) == 0)
				  EraseRect(&siderect);
				/* Draw two scorekeepers per line. */
				sx = (((i & 1) == 1) ? gamewinw / 2 : 0);
				/* Draw the scorekeeper's status. */
				MoveTo(sx + 10, siderect.top + 10);
				DrawText(scoredesc, 0, strlen(scoredesc));
				if (i == 0)
				  strcpy(last_status_score1[s2], scoredesc);
			}
			++i;
			/* Offset rectangle to next row. */
			if ((i & 1) == 0)
			  OffsetRect(&siderect, 0, gamesidescorehgt);
		}
	}
}

void
do_mouse_down_game(Point mouse, int mods)
{
	beep();
}

/* The construction planning window. */

void
create_construction_window()
{
	int done = FALSE, mainheight;
	Point cellsize;
	Rect listrect, tmprect;

	if (hasColorQD) {
		constructionwin = GetNewCWindow(wConstruction, NULL, (WindowPtr) -1L);
	} else {
		constructionwin = GetNewWindow(wConstruction, NULL, (WindowPtr) -1L);
	}
	constructbutton = GetNewControl(cConstructButton, constructionwin);
	researchbutton = GetNewControl(cResearchButton, constructionwin);
	SetPort(constructionwin);
	calc_construction_rects();
	run_length_text = TENew(&runlengthrect, &runlengthrect);
	set_construction_run_length(DEFAULT_RUN);
	editedrunlength = -1;
	/* Switch to a font for the lists. */
	TextFont(monaco);
	TextSize(9);
	/* Set up the list of all constructing units. */
	tmprect = unitlistrect;
	tmprect.right -= sbarwid;
	SetRect(&listrect, 0, 0, 1, 0);
	SetPt(&cellsize, 300, 12);
	/* Create the list of units itself. */
	construction_unit_list =
		LNew(&tmprect, &listrect, cellsize, 128, constructionwin,
			 FALSE, FALSE, FALSE, TRUE);
	/* Now set up the list of types. */
	tmprect = typelistrect;
	tmprect.right -= sbarwid;
	SetRect(&listrect, 0, 0, 1, 0);
	/* (should calc this from the desired font) */
	SetPt(&cellsize, 300, 12);
	construction_type_list =
		LNew(&tmprect, &listrect, cellsize, 128, constructionwin,
			 FALSE, FALSE, FALSE, TRUE);
	init_construction_lists();
	if (1 /* position construction at bottom of main screen */) {
		get_main_screen_size(NULL, &mainheight);
		tmprect = constructionwin->portRect;
		MoveWindow(constructionwin,
				   4,
				   mainheight - (tmprect.bottom - tmprect.top) - 3,
				   FALSE);
	}
	ShowWindow(constructionwin);
}

/* Build the list of constructing units and the list of constructible types. */

void
init_construction_lists()
{
	int u;
	Unit *unit;
	Cell tmpcell;

	/* Update the list of units. */
	LDoDraw(0, construction_unit_list);
	LDelRow(0, 0, construction_unit_list);
	SetPt(&tmpcell, 0, 0);
	/* Create the vector of constructing units, at a reasonable initial size. */
	if (constructor_units == NULL) {
		constructor_units = make_unit_vector(max(50, numunits));
	}
	clear_unit_vector(constructor_units);
	for_all_side_units(dside, unit) {
		maybe_add_unit_to_construction_list(unit);
	}
	LDoDraw(1, construction_unit_list);
	/* Update the list of types. */
	LDoDraw(0, construction_type_list);
	LDelRow(0, 0, construction_type_list);
	SetPt(&tmpcell, 0, 0);
	if (possibletypes == NULL)
	  possibletypes = (int *) xmalloc(numutypes * sizeof(int));
	numposstypes = 0;
	for_all_unit_types(u) {
		if (construction_possible(u) && type_allowed_on_side(u, dside)) {
			LAddRow(1, tmpcell.v, construction_type_list);
			constructible_desc(spbuf, dside, u, NULL);
			LSetCell(spbuf, strlen(spbuf), tmpcell, construction_type_list);
			++tmpcell.v;
			possibletypes[numposstypes++] = u;
		}
	}
	LDoDraw(1, construction_type_list);
	adjust_construction_controls();
}

void
reinit_construction_lists()
{
	init_construction_lists();
}

void
set_construction_run_length(int len)
{
	/* Do nothing if no change. */
    if (len == currunlength)
      return;
    currunlength = len;
	TESetSelect(0, 32767, run_length_text);
	TEDelete(run_length_text);
	sprintf(tmpbuf, "%d", len);
	TEInsert(tmpbuf, strlen(tmpbuf), run_length_text);
}

/* Draw the construction window by updating the lists and framing them. */

void
draw_construction()
{
	Rect tmprect;

	calc_construction_rects();
	TEUpdate(&(constructionwin->portRect), run_length_text);
	tmprect = runlengthrect;
	InsetRect(&tmprect, -1, -1);
	FrameRect(&tmprect);
	LUpdate(constructionwin->visRgn, construction_unit_list);
	tmprect = unitlistrect;
	InsetRect(&tmprect, -1, -1);
	FrameRect(&tmprect);
	LUpdate(constructionwin->visRgn, construction_type_list);
	tmprect = typelistrect;
	InsetRect(&tmprect, -1, -1);
	FrameRect(&tmprect);
	/* Maybe show the construct button as the default. */
	draw_construction_default();
}

/* Draw a heavy outline around the construction button. */

void
draw_construction_default()
{
	Rect tmprect;
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(constructionwin);
	tmprect = (*constructbutton)->contrlRect;
	PenSize(3, 3);
	InsetRect(&tmprect, -4, -4);
	if ((*constructbutton)->contrlHilite != 0) {
		PenMode(patBic);
	}
	FrameRoundRect(&tmprect, 16, 16);
	PenNormal();
	SetPort(oldport);
}

/* Figure out how to subdivide the construction window for the two lists. */

void
calc_construction_rects()
{
	int wid, hgt, divide;
	Rect tmprect;

	tmprect = constructionwin->portRect;
	runlengthrect = tmprect;
	runlengthrect.left = runlengthrect.right - 100;  runlengthrect.top = 5;
	runlengthrect.right -= 20;  runlengthrect.bottom = 25;
	wid = tmprect.right - tmprect.left - sbarwid;
	hgt = tmprect.bottom - tmprect.top - sbarwid;
	if (wid / 2 > 220 /* maxtypewid */) {
		divide = wid - 220;
	} else {
		divide = wid / 2;
	}
	SetRect(&unitlistrect, 0, constructtop, divide, hgt);
	InsetRect(&unitlistrect, constructmargin, constructmargin);
	SetRect(&typelistrect, divide, constructtop, wid, hgt);
	InsetRect(&typelistrect, constructmargin, constructmargin);
}

void
activate_construction(int activate)
{
	if (activate)
	  TEActivate(run_length_text);
	else
	  TEDeactivate(run_length_text);
	LActivate(activate, construction_unit_list);
	LActivate(activate, construction_type_list);
}

Unit *
get_selected_construction_unit()
{
	Point tmpcell;
	Unit *unit;

	SetPt(&tmpcell, 0, 0);
	if (LGetSelect(TRUE, &tmpcell, construction_unit_list)) {				
		if (tmpcell.v < constructor_units->numunits) {
			unit = (constructor_units->units)[tmpcell.v].unit;
			if (is_acting(unit))
			  return unit;
		}
	}
	return NULL;
}

int
get_selected_construction_type()
{
	Point tmpcell;

	SetPt(&tmpcell, 0, 0);
	if (LGetSelect(TRUE, &tmpcell, construction_type_list)) {				
		if (tmpcell.v < numposstypes) {
			return possibletypes[tmpcell.v];
		}
	}
	return NONUTYPE;
}

void
scroll_to_selected_construction_unit()
{
	Unit *unit;

	/* Beep and return if there are no maps open currently. */
	if (maplist == NULL) {
		beep();
		return;
	}
	unit = get_selected_construction_unit();
	if (unit != NULL && inside_area(unit->x, unit->y))
	  scroll_best_map_to_unit(unit);
}

/* Handle a click anywhere within the construction window. */

void
do_mouse_down_construction(Point mouse, int mods)
{
	ControlHandle control;
	short part;
	int u;
	Unit *unit;
	extern int modal_construction;
	extern WindowPtr window_behind_construction;

	part = FindControl(mouse, constructionwin, &control);
	if (control == constructbutton) {
		unit = get_selected_construction_unit();
		if (unit != NULL) {
			u = get_selected_construction_type();
			if (u != NONUTYPE) {
				push_build_task(unit, u, currunlength);
				execute_task(unit);
				update_construction_unit_list(unit);
				if (modal_construction && window_behind_construction != nil)
				  SelectWindow(window_behind_construction);
				window_behind_construction = NULL;
				modal_construction = FALSE;
				return;
			}
		}
	} else if (control == researchbutton) {
		unit = get_selected_construction_unit();
		if (unit != NULL) {
			u = get_selected_construction_type();
			if (u != NONUTYPE) {
				push_research_task(unit, u, u_tech_to_build(u));
				execute_task(unit);
				update_construction_unit_list(unit);
				if (modal_construction && window_behind_construction != nil)
				  SelectWindow(window_behind_construction);
				window_behind_construction = NULL;
				modal_construction = FALSE;
				return;
			}
		}
	} else if (PtInRect(mouse, &runlengthrect)) {
		TEClick(mouse, mods, run_length_text);
		/* (should switch this to be current item) */
	} else if (PtInRect(mouse, &unitlistrect)) {
		LClick(mouse, mods, construction_unit_list);
		/* Update the type list to show what could be built and in how long. */
		update_type_list_for_unit(get_selected_construction_unit());
	} else if (PtInRect(mouse, &typelistrect)) {
		LClick(mouse, mods, construction_type_list);
		/* Update the unit list to show what could build the type */
		update_unit_list_for_type(get_selected_construction_type());
	} else {
		/* Click was not in any useful part of the window. */ 
	}
}

int
do_key_down_construction(key)
int key;
{
	int len, runlength, i;
	char buffer[10];
	CharsHandle text;

	if (isdigit(key)) {
		TEKey(key, run_length_text);
		text = TEGetText(run_length_text);
		/* Pick out only the initial digits (up to 9). */
		len = min((*run_length_text)->teLength, 9);
		strncpy(buffer, *text, len);
		buffer[len] = '\0';
		runlength = atoi(buffer);
		if (between(1, runlength, 32767)) {
			currunlength = runlength;
			editedrunlength = runlength;
			return TRUE;
		}
	} else if (key == 13 || key == 3) {
		/* Pass enters and returns back to Dialog Manager. */
		return FALSE;
	} else {
		for (i = 0; i < numposstypes; ++i) {
			if (key == unitchars[possibletypes[i]]
				/* Skip over types that the selected unit can't build. */
				&& est_completion_time(get_selected_construction_unit(), possibletypes[i]) >= 0) {
				select_type_in_construction_window(possibletypes[i]);
				return TRUE;
			}
		}
		/* If not recognized as a unit type shortcut, hand it to the
		   run length field. */
		if (isalpha(key)) {
			beep();
			return FALSE;
		}
		TEKey(key, run_length_text);

	}
	return FALSE;
}

/* Highlight exactly one specific unit in the construction window, and unhighlight
   any others. */

void
select_unit_in_construction_window(Unit *unit)
{
	int i;
	Point tmpcell;

	for (i = 0; i < constructor_units->numunits; ++i) {
		SetPt(&tmpcell, 0, i);
		LSetSelect((unit == (constructor_units->units)[i].unit), tmpcell, construction_unit_list);
		LAutoScroll(construction_unit_list);
	}
	update_type_list_for_unit(get_selected_construction_unit());
}

void
select_type_in_construction_window(int u)
{
	int i;
	Point tmpcell;

	for (i = 0; i < numposstypes; ++i) {
		SetPt(&tmpcell, 0, i);
		LSetSelect((u == possibletypes[i]), tmpcell, construction_type_list);
		LAutoScroll(construction_type_list);
	}
	if (u == NONUTYPE)
	  return;
	update_unit_list_for_type(get_selected_construction_type());
}

/* Given a unit (which may be any unit), update the list of constructing units. */

void
update_construction_unit_list(Unit *unit)
{
	int i, u;
	Point tmpcell;

	if (constructionwin == nil)
	  return;
	u = get_selected_construction_type();
	/* We need to look for it even if it might not be ours, since it might
	   have been captured or otherwise lost, and needs to be removed. */
	for (i = 0; i < constructor_units->numunits; ++i) {
		if (unit == (constructor_units->units)[i].unit) {
			SetPt(&tmpcell, 0, i);
			if (is_active(unit)
				&& can_build_or_help(unit)
				&& side_controls_unit(dside, unit)) {
				construction_desc(spbuf, unit, u);
				LSetCell(spbuf, strlen(spbuf), tmpcell, construction_unit_list);
			} else {
				remove_unit_from_vector(constructor_units, unit, i);
				LDelRow(1, tmpcell.v, construction_unit_list);
			}
			return;
		}
	}
	/* Unit was not found, try to add it to the list. */
	maybe_add_unit_to_construction_list(unit);
}

void
maybe_add_unit_to_construction_list(Unit *unit)
{
	Point tmpcell;

	if (is_acting(unit)
		&& can_build_or_help(unit)
		&& side_controls_unit(dside, unit)) {
		/* Add this unit to the vector of constructing units. */
		constructor_units = add_unit_to_vector(constructor_units, unit, 0);
		/* (should sort and maybe rearrange list here) */
		/* Add a row at the end of the list. */
		SetPt(&tmpcell, 0, constructor_units->numunits - 1);
		LAddRow(1, constructor_units->numunits - 1, construction_unit_list);
		construction_desc(spbuf, unit, get_selected_construction_type());
		LSetCell(spbuf, strlen(spbuf), tmpcell, construction_unit_list);
	}
}

void
update_unit_list_for_type(int u)
{
	int i;
	Point tmpcell;
	Unit *unit;

	for (i = 0; i < constructor_units->numunits; ++i) {
		unit = (constructor_units->units)[i].unit;
		if (unit != NULL) {
			SetPt(&tmpcell, 0, i);
			if (is_acting(unit) && unit->side == dside) {
				construction_desc(spbuf, unit, u);
				LSetCell(spbuf, strlen(spbuf), tmpcell, construction_unit_list);
			} else {
/*				LDelRow(1, tmpcell.v, construction_unit_list); */
				LSetCell("", 0, tmpcell, construction_unit_list);
			}
		}
	}
	adjust_construction_controls();
}

void
update_construction_type_list()
{
	int u;

	if (constructionwin == nil)
	  return;
	u = get_selected_construction_type();
	update_type_list_for_unit(get_selected_construction_unit());
}

void
update_type_list_for_unit(Unit *unit)
{
	int i;
	Point tmpcell;

	for (i = 0; i < numposstypes; ++i) {
		constructible_desc(spbuf, dside, possibletypes[i], get_selected_construction_unit());
		SetPt(&tmpcell, 0, i);
		LSetCell(spbuf, strlen(spbuf), tmpcell, construction_type_list);
	}
	adjust_construction_controls();
}

/* Enable/disable controls according to whether the selected list elements can
   do construction activities. */

void
adjust_construction_controls()
{
	int u, canconstruct = FALSE, canresearch = FALSE, len;
	Unit *unit;

	unit = get_selected_construction_unit();
	if (unit != NULL) {
		u = get_selected_construction_type();
		if (u != NONUTYPE) {
			if (uu_acp_to_create(unit->type, u) > 0)
			  canconstruct = TRUE;
			if (uu_acp_to_research(unit->type, u) > 0)
			  canresearch = TRUE;
		}
	}
	HiliteControl(constructbutton, (canconstruct ? 0 : 255));
	HiliteControl(researchbutton, (canresearch ? 0 : 255));
	draw_construction_default();
	/* If there is doctrine on construction run length, use it to seed the
	   run length in the dialog. */
	if (editedrunlength < 0 && unit != NULL && u != NONUTYPE) {
		len = DEFAULT_RUN;
		if (dside->udoctrine[unit->type] != NULL
			&& dside->udoctrine[unit->type]->construction_run != NULL
			&& dside->udoctrine[unit->type]->construction_run[u] > 0) {
			len = (dside->udoctrine[unit->type])->construction_run[u];
		}
		set_construction_run_length(len);
	}
}

/* Resize the construction window to the given size. */

void
grow_construction(int h, int v)
{
	EraseRect(&constructionwin->portRect);
	SizeWindow(constructionwin, h, v, 1);
	adjust_construction_items();
	/* This will force a full redraw at the next update. */
	InvalRect(&constructionwin->portRect);
}					

/* Zooming "rightsizes" the window. */

void
zoom_construction(int part)
{
	int titleh, vislinesavail;
	Rect zoomrect;
	GDHandle zoomgd;

	EraseRect(&constructionwin->portRect);
	if (part == inZoomOut) {
		if (hasColorQD) {
			zoomgd = best_zoom_screen(&constructionwin->portRect);
			zoomrect = (*zoomgd)->gdRect;
			if (zoomgd == GetMainDevice()) {
				zoomrect.top += GetMBarHeight();
			}
		} else {
			/* If no Color QD, then there is only the one screen. */
			zoomrect = QD(screenBits).bounds;
			zoomrect.top += GetMBarHeight();
		}
		titleh = 20; /* (should calc) */
		zoomrect.top += titleh;
		InsetRect(&zoomrect, 4, 4);
		/* If not many units or types, shrink the zoomed window to fit. */
		vislinesavail = (zoomrect.bottom - zoomrect.top - sbarwid) / 15;
		if (0) {
			zoomrect.bottom = zoomrect.top + 20  * 15 + sbarwid;
		}
		(*((WStateDataHandle) ((WindowPeek) constructionwin)->dataHandle))->stdState = zoomrect;
	}
	ZoomWindow(constructionwin, part, (constructionwin == FrontWindow()));
	adjust_construction_items();
	/* This will force a full redraw at the next update. */
	InvalRect(&constructionwin->portRect);
}

/* Move and resize the list and text objects in the construction window. */

static void
adjust_construction_items()
{
	/* Recalculate size and position. */
	calc_construction_rects();
	/* Resize the run length text item. */
	(*run_length_text)->viewRect = runlengthrect;
	(*run_length_text)->destRect = runlengthrect;
	TECalText(run_length_text);
	/* Resize the unit list. */
	LSize(unitlistrect.right - unitlistrect.left - sbarwid,
		  unitlistrect.bottom - unitlistrect.top,
		  construction_unit_list);
	/* Move the type list (is this the approved way to do it?) */
	(*construction_type_list)->rView.left = typelistrect.left;
	LSize(typelistrect.right - typelistrect.left - sbarwid,
		  typelistrect.bottom - typelistrect.top,
		  construction_type_list);
}

/* The side renaming dialog includes places for all the different name-related
   properties of a side. */

void
side_rename_dialog(Side *side)
{
	short done = FALSE, changed = TRUE, ditem;
	Str255 tmpstr;
	DialogPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	win = GetNewDialog(dSideRename, NULL, (DialogPtr) -1L);
	while (!done) {
		if (changed) {
			/* Seed the items with the current side names. */
			GetDItem(win, diSideRenameName, &itemtype, &itemhandle, &itemrect);
			c2p((side->name ? side->name : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameFullName, &itemtype, &itemhandle, &itemrect);
			c2p((side->longname ? side->longname : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameAcronym, &itemtype, &itemhandle, &itemrect);
			c2p((side->shortname ? side->shortname : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameNoun, &itemtype, &itemhandle, &itemrect);
			c2p((side->noun ? side->noun : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenamePluralNoun, &itemtype, &itemhandle, &itemrect);
			c2p((side->pluralnoun ? side->pluralnoun : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameAdjective, &itemtype, &itemhandle, &itemrect);
			c2p((side->adjective ? side->adjective : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameEmblemName, &itemtype, &itemhandle, &itemrect);
			c2p((side->emblemname ? side->emblemname : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			GetDItem(win, diSideRenameColorScheme, &itemtype, &itemhandle, &itemrect);
			c2p((side->colorscheme ? side->colorscheme : ""), tmpstr);
			SetIText(itemhandle, tmpstr);
			ShowWindow(win);
			changed = FALSE;
		}
		draw_default_button(win, diSideRenameOK);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diSideRenameOK:
				/* Actually change the side's slots. */
				/* (should all go through networkable calls!) */
				GetDItem(win, diSideRenameName, &itemtype, &itemhandle, &itemrect);
				set_side_name(dside, dside, get_string_from_item(itemhandle));
				GetDItem(win, diSideRenameFullName, &itemtype, &itemhandle, &itemrect);
				side->longname = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenameAcronym, &itemtype, &itemhandle, &itemrect);
				side->shortname = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenameNoun, &itemtype, &itemhandle, &itemrect);
				side->noun = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenamePluralNoun, &itemtype, &itemhandle, &itemrect);
				side->pluralnoun = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenameAdjective, &itemtype, &itemhandle, &itemrect);
				side->adjective = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenameEmblemName, &itemtype, &itemhandle, &itemrect);
				side->emblemname = get_string_from_item(itemhandle);
				GetDItem(win, diSideRenameColorScheme, &itemtype, &itemhandle, &itemrect);
				side->colorscheme = get_string_from_item(itemhandle);
				/* Tweak the side menu. */
				update_side_menu(dside);
				/* Force redisplay of everything that might use any side names. */
				force_overall_update();
				/* Fall into next case. */
			case diSideRenameCancel:
				done = TRUE;
				break;
			case diSideRenameRandom:
				side->name = NULL;
				side->noun = NULL;
				/* always need to clear this cache before renaming... */
				side->pluralnoun = NULL;
				side->adjective = NULL;
				make_up_side_name(side);
				init_emblem_images(side);
				changed = TRUE;
				break;
		}
	}
	DisposDialog(win);
}

/* Unit naming/renaming. */

int
unit_rename_dialog(Unit *unit)
{
	short done = FALSE, ditem;
	char *newname;
	char *namer = unit_namer(unit);
	Str255 tmpstr;
	DialogPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	if (unit == NULL)
	  return FALSE;
	win = GetNewDialog(dRename, NULL, (DialogPtr) -1L);
	/* Seed the text item with the original name. */
	newname = unit->name;
	if (newname == NULL)
	  newname = "";
	GetDItem(win, diRenameName, &itemtype, &itemhandle, &itemrect);
	c2p(newname, tmpstr);
	SetIText(itemhandle, tmpstr);
	/* Gray out the random renaming button if no namers available. */
	GetDItem(win, diRenameRandom, &itemtype, &itemhandle, &itemrect);
	HiliteControl((ControlHandle) itemhandle, (!empty_string(namer) ? 0 : 255));
	ShowWindow(win);
	while (!done) {
		draw_default_button(win, diRenameOK);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diRenameOK:
				GetDItem(win, diRenameName, &itemtype, &itemhandle, &itemrect);
				set_unit_name(dside, unit, get_string_from_item(itemhandle));
				/* Fall into the next case. */
			case diRenameCancel:
				done = TRUE;
				break;
			case diRenameRandom:
				newname = propose_unit_name(unit);
				if (!empty_string(newname)) {
					GetDItem(win, diRenameName, &itemtype, &itemhandle, &itemrect);
				 	c2p(newname, tmpstr);
					SetIText(itemhandle, tmpstr);
				}
				break;
		}
	}
	DisposDialog(win);
	return TRUE;
}

/* Unit closeups. */

UnitCloseup *
find_unit_closeup(Unit *unit)
{
	UnitCloseup *unitcloseup;

	for_all_unit_closeups(unitcloseup) {
		if (unitcloseup->unit == unit
			&& unitcloseup->window
			&& ((WindowPeek) unitcloseup->window)->visible)
		  return unitcloseup;
	}
	return NULL;
}

void
create_unit_closeup(Unit *unit)
{
	int u, w, h;
	WindowPtr win;
	UnitCloseup *unitcloseup = (UnitCloseup *) xmalloc(sizeof(UnitCloseup));

	if (!active_display(dside) || unit == NULL)
	  return;
	DGprintf("Creating a closeup of %s\n", unit_desig(unit));
	u = unit->type;
	unitcloseup->unit = unit;
	if (hasColorQD) {
		win = GetNewCWindow(wUnitCloseup, nil, (WindowPtr) -1L);
	} else {
		win = GetNewWindow(wUnitCloseup, nil, (WindowPtr) -1L);
	}
	unitcloseup->window = win;
	stagger_window(unitcloseup->window, &lastunitcloseuph, &lastunitcloseupv);
	preferred_closeup_size(u, &w, &h);
	SizeWindow(win, w, h, 1);
	sprintf(spbuf, "Closeup on %s", short_unit_handle(unit));
	add_window_menu_item(spbuf, win);
	unitcloseup->next = unitcloseuplist;
	unitcloseuplist = unitcloseup;
	/* We're now ready to show this closeup to the world. */
	ShowWindow(win);
}

/* Compute the right size for a unit's closeup.  More complicated units
   with more state get bigger closeups. */

void
preferred_closeup_size(int u, int *widp, int *hgtp)
{
	int wid = closeupwinwid, hgt, m, u2, count;

	hgt = 4 + 32 + 4 + 3 * closeupspacing;
	if (nummrows == NULL) {
		/* Compute and cache the space needed to display each unit's supply. */
		nummrows = xmalloc(numutypes);
		for_all_unit_types(u2) {
			nummrows[u2] = count = 0;
			for_all_material_types(m) {
				if (um_storage_x(u2, m) > 0)
		  		  ++count;
		  	}
		  	if (count > 0) {
		  		nummrows[u2] = max(1, (count + (m_per_row - 1)) / m_per_row);
		  	}
		}
	}
	hgt += nummrows[u] * closeupspacing;
	if (u_acp(u) > 0)
	  hgt += 5 * closeupspacing;
	/* (should make some room for tooling state) */
	*widp = wid;  *hgtp = hgt;
}

/* Draw all the fields and displays in a unit closeup. */

void
draw_unit_closeup(UnitCloseup *unitcloseup)
{
	int u, m, sx = 4, sy, count, sx2, sy2;
	char tmpbuf[BUFSIZE];
	Rect tmprect;
	GrafPtr oldport;
	WindowPtr win = unitcloseup->window;
	Unit *unit = unitcloseup->unit;

	if (!active_display(dside))
	  return;
	if (!in_play(unit) || !(side_controls_unit(dside, unit) || endofgame)) {
		/* If the unit is no longer alive and ours, shut down the window. */
		remove_window_menu_item(win);
		destroy_unit_closeup(unitcloseup);
		HideWindow(win);
		return;
	}
	GetPort(&oldport);
	SetPort(win);
	/* Switch to a small fixed-width font. */
	TextFont(monaco);
	TextSize(9);
	EraseRect(&win->portRect);
	u = unit->type;
	/* Draw the unit's image. */
	SetRect(&tmprect, sx, sx, sx + 32, sx + 32); 
	EraseRect(&tmprect);
	draw_unit_image(win, tmprect.left, tmprect.top,
					tmprect.right - tmprect.left, tmprect.bottom - tmprect.top,
					u, side_number(unit->side), !completed(unit));
	/* Draw the unit's name. */
	name_or_number(unit, tmpbuf);
	MoveTo(44, 4 + closeupspacing);
	DrawText(tmpbuf, 0, strlen(tmpbuf));
	if (Debug || DebugG || DebugM) {
		sprintf(tmpbuf, " %d ", unit->id);
		MoveTo(closeupwinwid - 30, 15);
		DrawText(tmpbuf, 0, strlen(tmpbuf));
	}
	/* Draw the unit's side and type. */
	side_and_type_name(tmpbuf, dside, u, unit->side);
	MoveTo(44, 4 + closeupspacing * 2);
	DrawText(tmpbuf, 0, strlen(tmpbuf));
	/* Draw the unit's location. */
	if (unit->z != 0) {
		sprintf(tmpbuf, "; alt %d", unit->z);
	} else {
		strcpy(tmpbuf, "");
	}
	sprintf(spbuf, "at %d,%d%s", unit->x, unit->y, tmpbuf);
	if (unit->transport != NULL) {
		sprintf(tmpbuf, "In %s (%s)", short_unit_handle(unit->transport), spbuf);
	} else {
		strcpy(tmpbuf, spbuf);
	}
	sy = 4 + 32 + 4 + closeupspacing;
	MoveTo(sx, sy);
	DrawText(tmpbuf, 0, strlen(tmpbuf));
	/* Draw the unit's hit points. */
	hp_desc(tmpbuf, unit, TRUE);
	sy += closeupspacing;
	MoveTo(sx, sy);
	DrawText(tmpbuf, 0, strlen(tmpbuf));
	/* Draw the unit's current ACP, if applicable. */
	if (u_acp(u) > 0) {
		acp_desc(tmpbuf, unit, TRUE);
		sy += closeupspacing;
		MoveTo(sx, sy);
		DrawText(tmpbuf, 0, strlen(tmpbuf));
	}
	/* Draw the unit's supplies. */
	count = 0;
	if (nummrows[u] > 0)
	  sy += closeupspacing;
	for_all_material_types(m) {
		if (um_storage_x(u, m) > 0) {
			strcpy(tmpbuf, m_type_name(m));
			sx2 = sx + (count % m_per_row) * (closeupwinwid / m_per_row);
			sy2 = sy + (count / m_per_row) * closeupspacing;
			MoveTo(sx2, sy2);
			DrawText(tmpbuf, 0, strlen(tmpbuf));
			sprintf(tmpbuf, "%d/%d", unit->supply[m], um_storage_x(u, m));
			MoveTo(sx2 + (closeupwinwid / m_per_row - 4 - 4 - TextWidth(tmpbuf, 0, strlen(tmpbuf))), sy2);
			DrawText(tmpbuf, 0, strlen(tmpbuf));
			++count;
		}
	}
	if (nummrows[u] > 0)
	  sy += (nummrows[u] - 1) * closeupspacing; 
	/* Draw the unit's plan, if it has one. */
	if (unit->plan) {
		Task *task;
		extern char *plantypenames[];
		Plan *plan = unit->plan;
		
		sprintf(tmpbuf, "Plan: %s", plantypenames[plan->type]);
    	if (plan->waitingfortasks)
    	  strcat(tmpbuf, " [wait]");
    	if (plan->delayed) 
    	  strcat(tmpbuf, " [delay]");
    	if (plan->reserve)
    	  strcat(tmpbuf, " [resv]");
    	if (plan->asleep)
    	  strcat(tmpbuf, " [aslp]");
    	if (plan->aicontrol)
    	  strcat(tmpbuf, " [dlgt]");
    	if (plan->supply_is_low)
    	  strcat(tmpbuf, " [low]");
    	if (plan->supply_alarm)
    	  strcat(tmpbuf, " [alrm]");
		sy += closeupspacing;
		MoveTo(sx, sy);
		DrawText(tmpbuf, 0, strlen(tmpbuf));
    	if (plan->maingoal) {
    		/* (should use a "goal_desc" routine) */
    		strcpy(tmpbuf, goal_desig(plan->maingoal));
			sy += closeupspacing;			
			MoveTo(sx, sy);
			DrawText(tmpbuf, 0, strlen(tmpbuf));
    	}
    	if (plan->formation) {
    		/* (should use a "goal_desc" routine) */
    		strcpy(tmpbuf, goal_desig(plan->formation));
			sy += closeupspacing;			
			MoveTo(sx, sy);
			DrawText(tmpbuf, 0, strlen(tmpbuf));
    	}
    	for_all_tasks(plan, task) {
			task_desc(tmpbuf, task);
			sy += closeupspacing;			
			MoveTo(sx, sy);
			DrawText(tmpbuf, 0, strlen(tmpbuf));
		}
	}
	SetPort(oldport);
}


UnitCloseup *
unit_closeup_from_window(WindowPtr win)
{
	UnitCloseup *unitcloseup;
	
	for_all_unit_closeups(unitcloseup) {
		if (unitcloseup->window == win)
		  return unitcloseup;
	}
	return NULL;
}

int
do_mouse_down_unit_closeup(UnitCloseup *unitcloseup, Point mouse, int mods)
{
	ControlHandle control;
	short part;
	WindowPtr window = unitcloseup->window;

	part = FindControl(mouse, window, &control);
	if (0 /* some control */) {
	} else {
		/* This just forces a redraw of the window - kind of crude. */
		update_unit_display(dside, unitcloseup->unit, TRUE);
		return TRUE;
	}
}

void
destroy_unit_closeup(UnitCloseup *unitcloseup)
{
	UnitCloseup *unitcloseup2;
	
	if (unitcloseuplist == unitcloseup) {
		unitcloseuplist = unitcloseup->next;
	} else {
		for_all_unit_closeups(unitcloseup2) {
			if (unitcloseup2->next == unitcloseup) {
				unitcloseup2->next = unitcloseup->next;
			}
		}
	}
	/* (should destroy substructs) */
	free(unitcloseup);
}

/* History window. */

int maxvishistlines = 200;

void
create_history_window()
{
	Rect vscrollrect;

	historywin = GetNewWindow(wHistory, NULL, (WindowPtr) -1L);
	/* (should calc max based on size of font and height of screen) */
	histcontents = (HistEvent **) xmalloc(maxvishistlines * sizeof(HistEvent *));
	vscrollrect = historywin->portRect;
	vscrollrect.top -= 1;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	histvscrollbar = NewControl(historywin, &vscrollrect, "\p", TRUE,
			 					0, 0, 100, scrollBarProc, 0L);
	firstvisevt = history->next;
	secondvisevt = firstvisevt->next;
	update_total_hist_lines();
	set_history_scrollbar();
	ShowWindow(historywin);
}

void
calc_history_layout()
{
	update_total_hist_lines();
	set_history_scrollbar();
}

void
update_total_hist_lines()
{
	HistEvent *hevt;
	
	total_history_lines = 0;
	for (hevt = history->next; hevt != history; hevt = hevt->next) {
		if (side_in_set(dside, hevt->observers)) {
			if (hevt->startdate != hevt->prev->startdate) ++total_history_lines;
			++total_history_lines;
		}
	}
}

void
set_history_scrollbar()
{
	int hgt, oldmax;
	HistEvent *nexthevt;

	hgt = historywin->portRect.bottom - historywin->portRect.top;
	numvishistlines = (hgt - history_line_spacing - sbarwid) / history_line_spacing;
	oldmax = GetCtlMax(histvscrollbar);
	SetCtlMax(histvscrollbar, max(0, total_history_lines - numvishistlines + 1));
	HiliteControl(histvscrollbar, (numvishistlines < total_history_lines ? 0 : 255));
	/* If the thumb was at max, move it to the new max. */
	if (GetCtlValue(histvscrollbar) == oldmax) {
		SetCtlValue(histvscrollbar, GetCtlMax(histvscrollbar));
		if (GetCtlValue(histvscrollbar) == 0) {
			firstvisevt = history->next;
			secondvisevt = firstvisevt->next;
		} else {
			firstvisevt = get_nth_history_line(dside, GetCtlValue(histvscrollbar), &nexthevt);
			secondvisevt = nexthevt;
		}
	}
}

void
draw_history()
{
	int i, headdate;
    HistEvent *hevt;
	int numchars;
	char *datestr, hdatebuf[100];

	if (!active_display(dside) || historywin == nil)
	  return;
	/* Build up the array of events and dates to draw. */
	numhistcontents = 0;
	hevt = firstvisevt;
	if (hevt == NULL) {
		histcontents[numhistcontents++] = NULL;
		hevt = secondvisevt;
	}
	for (; hevt != history; hevt = hevt->next) {
		if (numhistcontents >= numvishistlines)
		  break;
		if (side_in_set(dside, hevt->observers)) {
			if (numhistcontents > 0
			    && histcontents[numhistcontents - 1] != NULL
			    && hevt->startdate != histcontents[numhistcontents - 1]->startdate) {
				histcontents[numhistcontents++] = NULL;
			}
			histcontents[numhistcontents++] = hevt;
		}
	}
	/* Draw the header line. */
	MoveTo(2, 10);
	headdate = (firstvisevt ? firstvisevt : secondvisevt)->startdate; 
	/* (should be relative) */
	datestr = absolute_date_string(headdate);
	sprintf(hdatebuf, "(%s)", datestr);
	/* (should clip to drawing only visible chars) */
	numchars = strlen(hdatebuf);
	DrawText(hdatebuf, 0, numchars);
	/* Now draw each event or date. */
	for (i = 0; i < numhistcontents; ++i) {
		if (histcontents[i] != NULL) {
			draw_historical_event(histcontents[i], i);
		} else {
			draw_historical_date(histcontents[i+1], i);
		}
	}
}

void
draw_historical_event(HistEvent *hevt, int y)
{
	int hgt, pos, numchars;
	char hevtbuf[500];

	if (hevt == NULL)
	  return;
	pos = history_line_spacing * y + history_top_line_height;
	hgt = historywin->portRect.bottom - historywin->portRect.top;
	/* Don't draw the line if it will intrude on the bottom scrollbar. */
	if (pos + history_line_spacing > hgt - sbarwid)
	  return;
	MoveTo(20, pos);
	historical_event_desc(dside, hevt, hevtbuf);
	/* (should clip to drawing only visible chars) */
	numchars = strlen(hevtbuf);
	DrawText(hevtbuf, 0, numchars);
}

void
draw_historical_date(HistEvent *hevt, int y)
{
	int numchars;
	char *datestr, hdatebuf[100];

	if (hevt == NULL)
	  return;
	MoveTo(2, history_line_spacing * y + history_top_line_height);
	/* (should be relative) */
	datestr = absolute_date_string(hevt->startdate);
	strcpy(hdatebuf, datestr);
	/* (should clip to drawing only visible chars) */
	numchars = strlen(hdatebuf);
	DrawText(hdatebuf, 0, numchars);
}

void
update_history_window(HistEvent *hevt)
{
	HistEvent *prevfirst, *prevsecond;

	prevfirst = firstvisevt;
	prevsecond = secondvisevt;
	SetPort(historywin);
	calc_history_layout();
	if (firstvisevt != prevfirst
	    || secondvisevt != prevsecond
	    || numvishistlines > total_history_lines) {
		force_update(historywin);
	}
}

static pascal void
history_scroll_fn(ControlHandle control, short code)
{
	int curvalue, maxvalue, pagesize, jump;

	curvalue = GetCtlValue(control);
	maxvalue = GetCtlMax(control);
	pagesize = numvishistlines;
	switch (code) {
		case inPageDown:
			jump = max(1, pagesize - 2);
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = min(-1, - (pagesize - 2));
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	curvalue = max(min(curvalue + jump, maxvalue), 0);
	SetCtlValue(control, curvalue);
}

void
do_mouse_down_history(Point mouse, int mods)
{
	HistEvent *prevfirst, *prevsecond, *nexthevt;
	ControlHandle control;
	short part, value;

	if (history_scroll_proc == NULL)
	  history_scroll_proc = NewControlActionProc(history_scroll_fn);

	part = FindControl(mouse, historywin, &control);
	if (control == histvscrollbar) {
		prevfirst = firstvisevt;
		prevsecond = secondvisevt;
		switch (part) {
			case inThumb:
				part = TrackControl(control, mouse, NULL);
				break;
			default:
				part = TrackControl(control, mouse, history_scroll_proc);
				break;
		}
		value = GetCtlValue(control);
		firstvisevt = get_nth_history_line(dside, value, &nexthevt);
		secondvisevt = nexthevt;
		if (firstvisevt != prevfirst || secondvisevt != prevsecond) {
			force_update(historywin);
		}
	} else {
		/* anything to do here? */
	}
}

/* Grow/shrink the history window to the given size. */

void
grow_history(int h, int v)
{
	EraseRect(&historywin->portRect);
	SizeWindow(historywin, h, v, 1);
	move_history_scrollbar(h, v);
	/* This will force a full redraw at the next update. */
	InvalRect(&historywin->portRect);
}					

/* Zoom the history window to its best maximal size. */

void
zoom_history(int part)
{
	int titleh, vislinesavail;
	Rect zoomrect;
	GDHandle zoomgd;

	EraseRect(&historywin->portRect);
	if (part == inZoomOut) {
		if (hasColorQD) {
			zoomgd = best_zoom_screen(&historywin->portRect);
			zoomrect = (*zoomgd)->gdRect;
			if (zoomgd == GetMainDevice()) {
				zoomrect.top += GetMBarHeight();
			}
		} else {
			/* If no Color QD, then there is only one screen. */
			zoomrect = QD(screenBits).bounds;
			zoomrect.top += GetMBarHeight();
		}
		titleh = 20; /* (should calc) */
		zoomrect.top += titleh;
		InsetRect(&zoomrect, 4, 4);
		/* If not much history, shrink the zoomed window to fit. */
		vislinesavail = (zoomrect.bottom - zoomrect.top - sbarwid) / history_line_spacing;
		update_total_hist_lines();
		if (vislinesavail > total_history_lines) {
			zoomrect.bottom = zoomrect.top + total_history_lines * history_line_spacing + sbarwid;
		}
		(*((WStateDataHandle) ((WindowPeek) historywin)->dataHandle))->stdState = zoomrect;
	}
	ZoomWindow(historywin, part, (historywin == FrontWindow()));
	move_history_scrollbar(window_width(historywin), window_height(historywin));
	/* This will force a full redraw at the next update. */
	InvalRect(&historywin->portRect);
}

void
move_history_scrollbar(int h, int v)
{
	MoveControl(histvscrollbar, h - sbarwid, 0);
	SizeControl(histvscrollbar, sbarwid + 1, v - sbarwid + 1);
	set_history_scrollbar();
}

/* notice window. */

/* This is the top-level access to bring up the notice window, can be called
   anywhere, anytime. */

void
notice_dialog()
{
	if (noticewin == nil) {
		create_notice_window();
	}
	ShowWindow(noticewin);
	SelectWindow(noticewin);
}

void
create_notice_window()
{
	int h, v, mainheight;
	Rect destrect, viewrect, vscrollrect, tmprect;

	/* Create the window, color if possible, since images may be in color. */
	if (hasColorQD) {	
		noticewin = GetNewCWindow(wNotice, NULL, (WindowPtr) -1L);
	} else {
		noticewin = GetNewWindow(wNotice, NULL, (WindowPtr) -1L);
	}
	SetPort(noticewin);
	/* All text will be in Times. */
	/* (should these be choosable?) */
	TextFont(times);
	/* Set up the notice text. */
	TextSize(14);
	h = window_width(noticewin);  v = window_height(noticewin);
	SetRect(&viewrect, 5, 5, h - sbarwid, v - sbarwid); 
	destrect = viewrect;
	notice_text = TENew(&destrect, &destrect);
	/* Set up a vertical scrollbar. */
	vscrollrect = noticewin->portRect;
	vscrollrect.top = 5;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	notice_v_scrollbar =
		NewControl(noticewin, &vscrollrect, "\p", TRUE, 0, 0, 0, scrollBarProc, 0L);
	HiliteControl(notice_v_scrollbar, 0);
	add_window_menu_item("Notices", noticewin);
	if (1 /* position notices at bottom of main screen */) {
		get_main_screen_size(NULL, &mainheight);
		tmprect = noticewin->portRect;
		MoveWindow(noticewin,
				   4,
				   mainheight - (tmprect.bottom - tmprect.top) - 3,
				   FALSE);
	}
}

void
append_notice(char *str)
{
	/* Delete old notices. */
	if (((*notice_text)->teLength) > 30000) {
		TESetSelect(0, (*notice_text)->teLength - 30000, notice_text);
		TEDelete(notice_text);
	}
#ifdef THINK_C
	/* Hack up newlines so that TextEdit recognizes them. */
	{
		int i;
	
		for (i = 0; i < strlen(str); ++i) {
			if (str[i] == '\n')
			  str[i] = '\r';
		}
	}
#endif
	TESetSelect(32767, 32767, notice_text);
	TEInsert(str, strlen(str), notice_text);
	TEInsert("\r", strlen("\r"), notice_text);
	TESetSelect(32767, 32767, notice_text);
	(*notice_text)->destRect = (*notice_text)->viewRect;
	/* Update on the screen. */
	adjust_notice_scrollbar();
	draw_notice();
}

void
draw_notice()
{
	Rect tmprect;
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(noticewin);
	SetRect(&tmprect, 5, 40, 5 + 32, 40 + 32);
	EraseRect(&tmprect);
	TextSize(14);
	TEUpdate(&(noticewin->portRect), notice_text);
	SetPort(oldport);
	adjust_notice_scrollbar();
}

void
adjust_notice_scrollbar()
{
	int lines, oldmax, newmax, oldvalue, newvalue;

	oldvalue = GetCtlValue(notice_v_scrollbar);
	oldmax = GetCtlMax(notice_v_scrollbar);
	lines = (*notice_text)->nLines;
	/* Account for a return at the end of the text. */
	if (*(*(*notice_text)->hText + (*notice_text)->teLength - 1) == 0x0d)
	  ++lines;
	newmax = lines - (((*notice_text)->viewRect.bottom - (*notice_text)->viewRect.top)
					 / (*notice_text)->lineHeight);
	if (newmax < 0)
	  newmax = 0;
	SetCtlMax(notice_v_scrollbar, newmax);
	if (oldvalue == oldmax) {
		/* If the thumb was at max, move it to the new max. */
		newvalue = newmax;
	} else {
		/* Otherwise adjust it proportionally. */
		newvalue = ((*notice_text)->viewRect.top - (*notice_text)->destRect.top)
					/ (*notice_text)->lineHeight;
		if (newvalue < 0)
		  newvalue = 0;
		if (newvalue > newmax)
		  newvalue = newmax;
	}
	SetCtlValue(notice_v_scrollbar, newvalue);
	TEScroll(0, ((*notice_text)->viewRect.top - (*notice_text)->destRect.top)
				 - (GetCtlValue(notice_v_scrollbar) * (*notice_text)->lineHeight),
			 notice_text);
}

void
activate_notice(int activate)
{
	HiliteControl(notice_v_scrollbar, (activate ? 0 : 255));
	if (activate)
	  TEActivate(notice_text);
	else
	  TEDeactivate(notice_text);
}

static pascal void
notice_vscroll_fn(ControlHandle control, short code)
{
	int oldvalue, curvalue, minvalue, maxvalue, pagesize, jump;

	curvalue = GetCtlValue(control);
	minvalue = GetCtlMin(control);
	maxvalue = GetCtlMax(control);
	pagesize = ((*notice_text)->viewRect.bottom - (*notice_text)->viewRect.top) /
				(*notice_text)->lineHeight;
	if (pagesize > 1)
	  pagesize -= 1;
	switch (code) {
		case inPageDown:
			jump = pagesize;
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = - pagesize;
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	oldvalue = curvalue;
	curvalue = max(min(curvalue + jump, maxvalue), minvalue);
	SetCtlValue(control, curvalue);
	/* Calculate the actual jump and use it to adjust the text. */
	jump = curvalue - oldvalue;
	if (jump != 0)
	  TEScroll(0, - jump * (*notice_text)->lineHeight, notice_text);
}

/* Respond to an event occurring in the notice window. */

void
do_mouse_down_notice(Point mouse, int mods)
{
	ControlHandle control;
	short part, value;

	if (notice_vscroll_proc == NULL)
	  notice_vscroll_proc = NewControlActionProc(notice_vscroll_fn);

	part = FindControl(mouse, noticewin, &control);
	if (control == notice_v_scrollbar) {
		if (part != 0) {
			switch (part) {
				case inPageDown:
				case inDownButton:
				case inPageUp:
				case inUpButton:
					value = TrackControl(control, mouse, notice_vscroll_proc);
					break;
				case inThumb:
					value = GetCtlValue(control);
					if ((part = TrackControl(control, mouse, nil)) != 0) {
						value -= GetCtlValue(control);
						if (value != 0) {
							TEScroll(0, value * (*notice_text)->lineHeight, notice_text);
						}
					}
					break;
			}
		}
	} else if (PtInRect(mouse, &((*notice_text)->viewRect))) {
		TEClick(mouse, 0, notice_text);
	}
}

void
grow_notice(int h, int v)
{
	EraseRect(&noticewin->portRect);
	SizeWindow(noticewin, h, v, 1);
	MoveControl(notice_v_scrollbar, h - sbarwid, 5);
	SizeControl(notice_v_scrollbar, sbarwid + 1, v - 5 - sbarwid + 1);
	(*notice_text)->viewRect.right = h - sbarwid;
	(*notice_text)->viewRect.bottom = v - sbarwid;
	(*notice_text)->destRect.right = h - sbarwid;
	TECalText(notice_text);
	InvalRect(&noticewin->portRect);
}					

void
zoom_notice(int part)
{
	int titleh, h, v;
	Rect zoomrect;
	GDHandle gd, zoomgd;

	EraseRect(&noticewin->portRect);
	if (part == inZoomOut) {
		if (hasColorQD) {
			zoomgd = best_zoom_screen(&noticewin->portRect);
			zoomrect = (*zoomgd)->gdRect;
			if (zoomgd == GetMainDevice()) {
				zoomrect.top += GetMBarHeight();
			}
			InsetRect(&zoomrect, 3, 3);
		} else {
			/* If no Color QD, then there is only the one screen. */
			zoomrect = QD(screenBits).bounds;
			zoomrect.top += GetMBarHeight();
			InsetRect(&zoomrect, 4, 4);
		}
		titleh = 20; /* (should calc) */
		zoomrect.top += titleh;
		(*((WStateDataHandle) ((WindowPeek) noticewin)->dataHandle))->stdState = zoomrect;
	}
	ZoomWindow(noticewin, part, (noticewin == FrontWindow()));
	h = window_width(noticewin);  v = window_height(noticewin);
	MoveControl(notice_v_scrollbar, h - sbarwid, 0);
	SizeControl(notice_v_scrollbar, sbarwid + 1, v - sbarwid + 1);
	adjust_notice_scrollbar();
	(*notice_text)->viewRect.right = h - sbarwid;
	(*notice_text)->viewRect.bottom = v - sbarwid;
	(*notice_text)->destRect.right = h - sbarwid;
	TECalText(notice_text);
	/* This will force a full redraw at the next update. */
	InvalRect(&noticewin->portRect);
}

/* scores window. */

/* This is the top-level access to bring up the scores window, can be called
   anywhere, anytime. */

void
scores_dialog()
{
    extern char *get_scores(Side *side);

	if (scoreswin == nil) {
		create_scores_window();
		append_scores(get_scores(dside));
	}
	ShowWindow(scoreswin);
	SelectWindow(scoreswin);
}

void
create_scores_window()
{
	int h, v;
	Rect destrect, viewrect, vscrollrect;

	/* Create the window, color if possible, since images may be in color. */
	if (hasColorQD) {	
		scoreswin = GetNewCWindow(wScores, NULL, (WindowPtr) -1L);
	} else {
		scoreswin = GetNewWindow(wScores, NULL, (WindowPtr) -1L);
	}
	SetPort(scoreswin);
	/* All text will be in Times. */
	/* (should these be choosable?) */
	TextFont(times);
	/* Set up the scores text. */
	TextSize(14);
	h = window_width(scoreswin);  v = window_height(scoreswin);
	SetRect(&viewrect, 5, 5, h - sbarwid, v - sbarwid); 
	destrect = viewrect;
	scores_text = TENew(&destrect, &destrect);
	/* Set up a vertical scrollbar. */
	vscrollrect = scoreswin->portRect;
	vscrollrect.top = 5;
	vscrollrect.bottom -= sbarwid - 1;
	vscrollrect.left = vscrollrect.right - sbarwid;
	vscrollrect.right += 1;
	scores_v_scrollbar =
		NewControl(scoreswin, &vscrollrect, "\p", TRUE, 0, 0, 0, scrollBarProc, 0L);
	HiliteControl(scores_v_scrollbar, 0);
	add_window_menu_item("scores", scoreswin);
}

void
append_scores(char *str)
{
	/* Delete old scoress. */
	if (((*scores_text)->teLength) > 30000) {
		TESetSelect(0, (*scores_text)->teLength - 30000, scores_text);
		TEDelete(scores_text);
	}
#ifdef THINK_C
	/* Hack up newlines so that TextEdit recognizes them. */
	{
		int i;
	
		for (i = 0; i < strlen(str); ++i) {
			if (str[i] == '\n')
			  str[i] = '\r';
		}
	}
#endif
	TESetSelect(32767, 32767, scores_text);
	TEInsert(str, strlen(str), scores_text);
	TEInsert("\r", strlen("\r"), scores_text);
	TESetSelect(32767, 32767, scores_text);
	(*scores_text)->destRect = (*scores_text)->viewRect;
	/* Update on the screen. */
	adjust_scores_scrollbar();
	draw_scores();
}

void
draw_scores()
{
	Rect tmprect;
	GrafPtr oldport;

	GetPort(&oldport);
	SetPort(scoreswin);
	SetRect(&tmprect, 5, 40, 5 + 32, 40 + 32);
	EraseRect(&tmprect);
	TextSize(14);
	TEUpdate(&(scoreswin->portRect), scores_text);
	SetPort(oldport);
	adjust_scores_scrollbar();
}

void
adjust_scores_scrollbar()
{
	int lines, oldmax, newmax, oldvalue, newvalue;

	oldvalue = GetCtlValue(scores_v_scrollbar);
	oldmax = GetCtlMax(scores_v_scrollbar);
	lines = (*scores_text)->nLines;
	/* Account for a return at the end of the text. */
	if (*(*(*scores_text)->hText + (*scores_text)->teLength - 1) == 0x0d)
	  ++lines;
	newmax = lines - (((*scores_text)->viewRect.bottom - (*scores_text)->viewRect.top)
					 / (*scores_text)->lineHeight);
	if (newmax < 0)
	  newmax = 0;
	SetCtlMax(scores_v_scrollbar, newmax);
	if (oldvalue == oldmax) {
		/* If the thumb was at max, move it to the new max. */
		newvalue = newmax;
	} else {
		/* Otherwise adjust it proportionally. */
		newvalue = ((*scores_text)->viewRect.top - (*scores_text)->destRect.top)
					/ (*scores_text)->lineHeight;
		if (newvalue < 0)
		  newvalue = 0;
		if (newvalue > newmax)
		  newvalue = newmax;
	}
	SetCtlValue(scores_v_scrollbar, newvalue);
	TEScroll(0, ((*scores_text)->viewRect.top - (*scores_text)->destRect.top)
				 - (GetCtlValue(scores_v_scrollbar) * (*scores_text)->lineHeight),
			 scores_text);
}

void
activate_scores(int activate)
{
	HiliteControl(scores_v_scrollbar, (activate ? 0 : 255));
	if (activate)
	  TEActivate(scores_text);
	else
	  TEDeactivate(scores_text);
}

static pascal void
scores_vscroll_fn(ControlHandle control, short code)
{
	int oldvalue, curvalue, minvalue, maxvalue, pagesize, jump;

	curvalue = GetCtlValue(control);
	minvalue = GetCtlMin(control);
	maxvalue = GetCtlMax(control);
	pagesize = ((*scores_text)->viewRect.bottom - (*scores_text)->viewRect.top) /
				(*scores_text)->lineHeight;
	if (pagesize > 1)
	  pagesize -= 1;
	switch (code) {
		case inPageDown:
			jump = pagesize;
			break;
		case inDownButton:
			jump = 1;
			break;
		case inPageUp:
			jump = - pagesize;
			break;
		case inUpButton:
			jump = -1;
			break;
		default:
			jump = 0;
			break;
	}
	oldvalue = curvalue;
	curvalue = max(min(curvalue + jump, maxvalue), minvalue);
	SetCtlValue(control, curvalue);
	/* Calculate the actual jump and use it to adjust the text. */
	jump = curvalue - oldvalue;
	if (jump != 0)
	  TEScroll(0, - jump * (*scores_text)->lineHeight, scores_text);
}

/* Respond to an event occurring in the scores window. */

void
do_mouse_down_scores(Point mouse, int mods)
{
	ControlHandle control;
	short part, value;

	if (scores_vscroll_proc == NULL)
	  scores_vscroll_proc = NewControlActionProc(scores_vscroll_fn);

	part = FindControl(mouse, scoreswin, &control);
	if (control == scores_v_scrollbar) {
		if (part != 0) {
			switch (part) {
				case inPageDown:
				case inDownButton:
				case inPageUp:
				case inUpButton:
					value = TrackControl(control, mouse, scores_vscroll_proc);
					break;
				case inThumb:
					value = GetCtlValue(control);
					if ((part = TrackControl(control, mouse, nil)) != 0) {
						value -= GetCtlValue(control);
						if (value != 0) {
							TEScroll(0, value * (*scores_text)->lineHeight, scores_text);
						}
					}
					break;
			}
		}
	} else if (PtInRect(mouse, &((*scores_text)->viewRect))) {
		TEClick(mouse, 0, scores_text);
	}
}

void
grow_scores(int h, int v)
{
	EraseRect(&scoreswin->portRect);
	SizeWindow(scoreswin, h, v, 1);
	MoveControl(scores_v_scrollbar, h - sbarwid, 5);
	SizeControl(scores_v_scrollbar, sbarwid + 1, v - 5 - sbarwid + 1);
	(*scores_text)->viewRect.right = h - sbarwid;
	(*scores_text)->viewRect.bottom = v - sbarwid;
	(*scores_text)->destRect.right = h - sbarwid;
	TECalText(scores_text);
	InvalRect(&scoreswin->portRect);
}					

void
zoom_scores(int part)
{
	int titleh, h, v;
	Rect zoomrect;
	GDHandle gd, zoomgd;

	EraseRect(&scoreswin->portRect);
	if (part == inZoomOut) {
		if (hasColorQD) {
			zoomgd = best_zoom_screen(&scoreswin->portRect);
			zoomrect = (*zoomgd)->gdRect;
			if (zoomgd == GetMainDevice()) {
				zoomrect.top += GetMBarHeight();
			}
			InsetRect(&zoomrect, 3, 3);
		} else {
			/* If no Color QD, then there is only the one screen. */
			zoomrect = QD(screenBits).bounds;
			zoomrect.top += GetMBarHeight();
			InsetRect(&zoomrect, 4, 4);
		}
		titleh = 20; /* (should calc) */
		zoomrect.top += titleh;
		(*((WStateDataHandle) ((WindowPeek) scoreswin)->dataHandle))->stdState = zoomrect;
	}
	ZoomWindow(scoreswin, part, (scoreswin == FrontWindow()));
	h = window_width(scoreswin);  v = window_height(scoreswin);
	MoveControl(scores_v_scrollbar, h - sbarwid, 0);
	SizeControl(scores_v_scrollbar, sbarwid + 1, v - sbarwid + 1);
	adjust_scores_scrollbar();
	(*scores_text)->viewRect.right = h - sbarwid;
	(*scores_text)->viewRect.bottom = v - sbarwid;
	(*scores_text)->destRect.right = h - sbarwid;
	TECalText(scores_text);
	/* This will force a full redraw at the next update. */
	InvalRect(&scoreswin->portRect);
}
