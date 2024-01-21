/* The main program of the curses interface to Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void low_send PARAMS ((int id, char *buf));
extern int low_receive PARAMS ((int *id, char *buf, int maxchars,
				int timeout));
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
extern char *get_scores PARAMS ((Side *side));
#include "cmdline.h"
#include "cconq.h"
extern void make_generic_image_data PARAMS ((ImageFamily *imf));

#ifdef THINK_C
/* This is to get the command line reader in Think C on the Mac. */
#include <console.h>
#endif /* THINK_C */

#ifdef __MWERKS__
#define MAC
/* This is to get the command line reader in Metrowerks on the Mac. */
#include <console.h>
#include <sioux.h>
#endif /* __MWERKS__ */

static void describe_help PARAMS ((int arg, char *key, char *buf));

int announced = FALSE;

char *announcemsg = NULL;

/* The side of the one human player. */

Side *dside = NULL;

/* move to where a change has occured */

int follow_action;

/* The location currently being examined. */

int curx, cury;

int tmpcurx, tmpcury;

/* The unit currently being examined, if any. */

Unit *curunit;

Unit *tmpcurunit;

/* The current interaction mode. */

enum mode mode;

/* The pushed interaction mode. */

enum mode prevmode;

/* The current input character. */

char inpch;

/* The command's numeric argument. */

int prefixarg;

/* These are used when asking for a unit type. */

char *ustr;
int *uvec;
int *bvec;

/* length and number of notice lines */

int nw, nh;

/* last current x and y (-1,-1 initially) */

int lastx, lasty;

/* current prompt on display */

char *text1;
char *text2;

/* data about string under construction */

int reqstrbeg;
int reqstrend;

/* The help window. */

struct ccwin *helpwin;

/* Cached help info. */

HelpNode *cur_help_node;

HelpNode *help_help_node;

HelpNode *commands_help_node;

HelpNode *topics_help_node;

struct ccwin *datewin;

struct ccwin *sideswin;

struct ccwin *toplineswin;

struct ccwin *clockwin;

/* Display of a piece of the world */

struct ccwin *mapwin;

/* Unit listing */

struct ccwin *listwin;

struct ccwin *closeupwin;

struct ccwin *sidecloseupwin;

/* Map dimensions. */

int mw, mh;

VP *mvp;

/* lower left of viewport in cell coords */

int vx, vy;

/* Boundaries of viewport in cell coords */

int vw, vh;

/* 1/2 (rounded down) of above values */

int vw2, vh2;

int lastvcx, lastvcy;

/* Num text lines in unit info window. */

int infoh;

/* List dimensions. */

int lw, lh;

/* Side list dimensions. */

int sh;

/* How to draw the map. */

int drawterrain;

/* Display units on the map? */

int drawunits;

/* Display unit names/numbers on the map? */

int drawnames;

int drawpeople;

enum listsides listsides;
int test;
int value;
int sorton;
int sortorder;

/* True when the display is up and running. */

int active;

/* Countdown for when multiple turns autofinishing. */

int autofinish_start;

int autofinish_count;

/* The main program just invokes everything else. */

int
main(argc, argv)
int argc;
char *argv[];
{
#ifdef THINK_C
    /* This is how Think C picks up a command line. */
    argc = ccommand(&argv);
#endif
#ifdef __MWERKS__
    /* This is how Metrowerks C picks up a command line. */
    argc = ccommand(&argv);
    /* Configure the SIOUX window. */
    SIOUXSettings.initializeTB = TRUE;
    SIOUXSettings.standalone = FALSE;
    SIOUXSettings.setupmenus = FALSE;
    SIOUXSettings.autocloseonquit = FALSE;
    SIOUXSettings.asktosaveonclose = TRUE;
    SIOUXSettings.showstatusline = TRUE;
#endif
#ifdef MAC
	_maccur_pgm_name = "Cconq";
#endif
    printf("\n              Welcome to Curses Xconq version %s\n\n",
	   version_string());
    init_library_path(NULL);
    print_any_news();
    printf("%s", license_string());
    clear_game_modules();
    init_data_structures();
    parse_command_line(argc, argv, general_options);
#ifdef DEBUGGING
    update_debugging();
#endif /* DEBUGGING */
    /* (should volunteer to restore saved game if one found) */
    load_all_modules();
    check_game_validity();
    parse_command_line(argc, argv, variant_options);
    set_variants_from_options();
    parse_command_line(argc, argv, player_options);
    /* Go through once more and complain about anything not used. */
    parse_command_line(argc, argv, leftover_options);
    set_players_from_options();
    make_trial_assignments();
    calculate_globals();
    run_synth_methods();
    final_init();
    assign_players_to_sides();
    /* Run through game calcs, but don't let anything move yet; this
       gives display code better values to work with. */
    run_game(0);
    init_display();
    init_interaction();
    redraw();
    init_signal_handlers();
    while (TRUE) {
#ifdef HAVE_SELECT
please somebody write this code
	run_game(1);
#else
	set_g_use_side_priority(TRUE);
	/* assumes no real-timeness */
	while (run_game(9) > 0)
	  ;
	maybe_handle_input(0);
#endif
    }
    /* Humor the compiler. */
    return 0;
}

/* Nonempty display name not actually used, but needed to keep things
   straight. */

Player *
add_default_player()
{
    Player *player = add_player();

    player->name = getenv("USER");
    player->displayname = "term";
    return player;
}

/* Set up the basic user interface for a side. */

void
init_ui(side)
Side *side;
{
    if (side_wants_display(side)) {
	if (dside == NULL) {
	    side->ui = (struct a_ui *) xmalloc(sizeof(int));
	    dside = side;
	    active = FALSE;
	    follow_action = FALSE;
	    cur_help_node = NULL;
	    mode = prevmode = SURVEY;
	    prefixarg = -1;
	} else {
	    fprintf(stderr, "More than one side wanted a display!\n");
	    exit(1);
	}
    }
}

/* Open display, create all the windows we'll need, do misc setup things,
   and initialize some globals to out-of-range values for recognition later. */

void
init_display()
{
    int x, y, i;

    if (dside == NULL) {
	fprintf(stderr, "No side wanted a display!\n");
	exit(1);
    }
    Dprintf("Will try to open screen as display...\n");
    /* Go through the standard curses setup sequence. */
    initscr();
    nonl();
    noecho();
    cbreak();
    clear();
    /* Set up random globals. */
    nw = min(BUFSIZE, 60);
    nh = 1;
    text1 = xmalloc (BUFSIZE);
    text2 = xmalloc (BUFSIZE);
    text1[0] = '\0';
    text2[0] = '\0';
    ustr = xmalloc (MAXUTYPES);
    uvec = (int *) xmalloc (MAXUTYPES * sizeof(int));
    bvec = (int *) xmalloc (MAXUTYPES * sizeof(int));
    /* Compute the division of the screen. */
    mw = (2 * COLS) / 3;
    infoh = 4;
    mh = LINES - 2 - infoh - 1;

    mvp = new_vp();
    /* Each cell is actually 2 chars or "pixels" wide.  This is the
       difference from the standard power==0 parameters, so we do
       a cheesy thing and tweak the array directly. */
    hws[0] = 2;
    set_view_power(mvp, 0);
    set_view_size(mvp, mw, mh);

    lw = COLS - mw - 1;

    sh = numsides + 1;
    /* The height of the list window is what's left over after making
       space for the side list and status lines. */
    lh = LINES - sh - 1 - 1;

    pick_a_focus(dside, &x, &y);
    set_view_focus(mvp, x, y);
    lastvcx = lastvcy = -1;
    /* Set default values for the display controls. */
    drawterrain = TRUE;
    drawunits = TRUE;
    drawnames = FALSE;  /* they clutter up the screen */
    listsides = allsides;
    curunit = NULL;
    /* Create all the windows. */
    toplineswin = create_window(0, 0, nw, 2);
    datewin = create_window(nw, 0, COLS - nw, 1);
    clockwin = create_window(nw, 1, COLS - nw, 1); 
    closeupwin = create_window(0, 2, mw, infoh);
    mapwin = create_window(0, 2 + infoh, mw, mh + 1);
    sideswin = create_window(mw + 1, 2, lw, sh);
    listwin = create_window(mw + 1, 2 + sh, lw, lh);
    /* Help window covers the screen. */
    helpwin = create_window(0, 0, COLS, LINES);
    /* Set up the initial scroll position of the map. */
    set_scroll();
    /* Make a buffer full of dashes, for use on mode lines. */
    if (dashbuffer == NULL) {
	dashbuffer = xmalloc(COLS + 1);
	for (i = 0; i < COLS; ++i)
	  dashbuffer[i] = '-';
	dashbuffer[COLS] = '\0';
    }
    init_ui_chars();
    /* Give the interface's state-saving mechanism a chance to preallocate
       anything it might want to use; allocation is not allowed during saving. */
    ui_save_state(dside);
    active = TRUE;
    Dprintf("Successfully opened display!\n");
}

void
init_interaction()
{
    curunit = NULL;
    find_next_and_look();
    if (curunit) {
	mode = MOVE;
    } else {
	mode = SURVEY;
	move_survey(mvp->vcx, mvp->vcy);
    }
    show_cursor();
    /* Set up some cconq-specific help nodes. */
    if (cur_help_node == NULL) {
	help_help_node =
	  add_help_node("help", describe_help, 0, first_help_node);
	commands_help_node =
	  add_help_node("commands", describe_commands, 0, first_help_node);
	topics_help_node =
	  add_help_node("topics", describe_topics, 0, first_help_node);
	cur_help_node = topics_help_node;
    }
    /* By default, require manual `return' to finish each turn. */
    set_autofinish(dside, FALSE);
}

/* Windows in curses are just simple bounding boxes. */

struct ccwin *
create_window(x, y, w, h)
int x, y, w, h;
{
    struct ccwin *newwin;

    if (x + w > COLS)
      w = COLS - x;
    if (y + h > LINES)
      h = LINES - y;
    DGprintf("Creating %dx%d window at %d,%d\n", w, h, x, y);

    newwin = (struct ccwin *) xmalloc(sizeof(struct ccwin));
    newwin->x = x;  newwin->y = y;
    newwin->w = w;  newwin->h = h;
    return newwin;
}

/* Generic input character acquisition. */

/* (should have a global that indicates whether this is in a modal
   place, then run_game not called on timeout, but waits until out
   of modal thing (perhaps aborts modal dialog automatically, then runs?) */

int
wait_for_char()
{
    show_cursor();
    /* Only take 7-bit chars. */
    inpch = getch() & 0177;
    return inpch;
}

void
maybe_handle_input(tmout)
int tmout;
{
    Unit *unit;

    if (active_display(dside)) {
	if (mode == MOVE) {
	    if (curunit != NULL
	    	&& in_play(curunit)
		&& (curunit->act
		&& curunit->act->acp > 0)  /* should be "above min"? */
	    	&& (curunit->plan
		    && !curunit->plan->asleep
		    && !curunit->plan->reserve
		    && !curunit->plan->delayed)
		) {
		/* should scroll over to unit */
	    } else {
		unit = autonext_unit_inbox(dside, curunit, mvp);
		if (unit
		    && unit->plan
		    && !unit->plan->asleep
		    && !unit->plan->reserve
		    && !unit->plan->delayed
		    && unit->plan->waitingfortasks
		    ) {
		    make_current(unit);
		} else {
		    make_current(autonext_unit(dside, curunit));
		}
	    }
	}
#ifdef HAVE_SELECT
please somebody write this code
#else
	wait_for_char();
#endif
	/* Program will not work without following due to compiler bug. (?) */
	Dprintf("key is %c", inpch); 
	/* Interpret the input. */
	interpret_input();
    }
}

void
interpret_input()
{
    int dir;

    if (inpch == REDRAW) {
	/* Redraw everything but leave the interaction state unchanged. */
	redraw();
    } else if (mode == HELP) {
	interpret_help();
    } else if (isdigit(inpch)) {
	/* Build up a numeric prefix argument. */
	if (prefixarg < 0) {
	    prefixarg = 0;
	} else {
	    prefixarg *= 10;
	}
	prefixarg += (inpch - '0');
	sprintf(text1, "Arg: %d", prefixarg);
	show_toplines();
    } else {
	clear_toplines();
	if ((dir = iindex(inpch, dirchars)) >= 0) {
	    if (prefixarg < 0)
	      prefixarg = 1;
	    do_dir_2(dir, prefixarg);
	} else if ((dir = iindex(lowercase(inpch), dirchars)) >= 0) {
	    if (prefixarg < 0)
	      prefixarg = 9999;
	    if (mode == SURVEY)
	      prefixarg = 10;
	    do_dir_2(dir, prefixarg);
	} else {
	    execute_command();
	}
	/* Reset the arg so we don't get confused next time around */
	prefixarg = -1;
    }
}

/* Interpret a direction character. */

void
do_dir_2(dir, n)
int dir, n;
{
    int nx, ny, rslt;

    if (mode == SURVEY) {
	if (point_in_dir_n(curx, cury, dir, n, &nx, &ny)) {
	    move_survey(nx, ny);
	} else if (n >= 2 && point_in_dir_n(curx, cury, dir, n / 2, &nx, &ny)) {
	    move_survey(nx, ny);
	} else {
	    xbeep();
	}
    } else if (curunit == NULL) {
	return;  /* no beep? */
    } else if (n > 1) {
	set_move_dir_task(curunit, dir, n);
    } else {
	if (point_in_dir(curx, cury, dir, &nx, &ny)) {
	    rslt = advance_into_cell(dside, curunit, nx, ny, unit_at(nx, ny));
	    if (!rslt)
	      xbeep();
	} else {
	    xbeep();
	}
    }
}

int
auto_attack_on_move(unit, unit2)
Unit *unit, *unit2;
{
    return TRUE;
}

void
move_survey(nx, ny)
int nx, ny;
{
    if (inside_area(nx, ny)) {
	make_current_at(nx, ny);
    } else {
	/* Complain if we're trying to move the cursor offworld. */
	xbeep();
    }
}

/* Ensure that given location is visible. */

void
put_on_screen(x, y)
int x, y;
{
    if (!in_middle(x, y)) {
	set_view_focus(mvp, x, y);
	center_on_focus(mvp);
	set_map_viewport();
	show_map();
	show_cursor();
    }
    show_closeup();
}

/* Prompt for a yes/no answer with a settable default. */

int
ask_bool(question, dflt)
char *question;
int dflt;
{
    char ch;

    prevmode = mode;
    mode = PROMPT;
    sprintf(text1, "%s [%s]", question, (dflt ? "yn" : "ny"));
    show_toplines();
    ch = wait_for_char();
    if (dflt ? (lowercase(ch) == 'n') : (lowercase(ch) == 'y'))
      dflt = !dflt;
    mode = prevmode;
    clear_toplines();
    return dflt;
}

/* Prompt for a type of a unit from player, maybe only allowing some types
   to be accepted.  Also allow specification of no unit type.  We do this
   by scanning the vector, building a string of chars and a vector of
   unit types, so as to be able to map back when done. */

int
ask_unit_type(prompt, possibles)
char *prompt;
short *possibles;
{
    char ch;
    int numtypes = 0, u;

    for_all_unit_types(u) {
	bvec[u] = 0;
	if (possibles == NULL || possibles[u]) {
	    bvec[u] = 1;
	    ustr[numtypes] = utype_name_n(u, 1)[0];
	    uvec[numtypes] = u;
	    numtypes++;
	}
    }
    if (numtypes == 0) {
	return NONUTYPE;
    } else {
	prevmode = mode;
	mode = PROMPT;
	ustr[numtypes] = '\0';
	sprintf(text1, "%s [%s] ", prompt, ustr);
	show_toplines();
    }
    while (1) {
	ch = wait_for_char();
	if (ch == ESCAPE) {
	    mode = prevmode;
	    clear_toplines();
	    return NONUTYPE;
	}
	if (iindex(ch, ustr) >= 0) {
	    mode = prevmode;
	    clear_toplines();
	    return uvec[iindex(ch, ustr)];
	}
	xbeep();
    }
}

/* Prompt for a type of a terrain from player, maybe only allowing some types
   to be accepted.  Also allow specification of no terrain type.  We do this
   by scanning the vector, building a string of chars and a vector of
   terrain types, so as to be able to map back when done. */

int
ask_terrain_type(prompt, possibles)
char *prompt;
short *possibles;
{
    char ch;
    int numtypes = 0, t, type;

    for_all_terrain_types(t) {
	bvec[t] = 0;
	if (possibles == NULL || possibles[t]) {
	    bvec[t] = 1;
	    ustr[numtypes]
	      = (t_char(t) ? t_char(t)[0] : t_type_name(t)[0]);
	    uvec[numtypes] = t;
	    numtypes++;
	}
    }
    if (numtypes == 0) {
	type = NONTTYPE;
    } else if (numtypes == 1) {
	type = uvec[0];
	bvec[type] = 0;
    } else {
	ustr[numtypes] = '\0';
	sprintf(text1, "%s [%s] ", prompt, ustr);
	show_toplines();
    }
    while (1) {
	ch = wait_for_char();
	if (ch == ESCAPE) {
	    clear_toplines();
	    return NONTTYPE;
	}
	if (iindex(ch, ustr) >= 0) {
	    clear_toplines();
	    return uvec[iindex(ch, ustr)];
	}
	xbeep();
    }
}

/* Ask for a direction. */

int
ask_direction(prompt, dirp)
char *prompt;
int *dirp;
{
    char ch;

    prevmode = mode;
    mode = PROMPTXY;
    sprintf(text1, "%s", prompt);
    sprintf(text2, " [direction keys]");
    show_toplines();
    save_cur();
    while (1) {
	ch = wait_for_char();
	if (ch == ESCAPE) {
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return FALSE;
	}
	*dirp = iindex(ch, dirchars);
	if (*dirp >= 0) {
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return TRUE;
	} else {
	    xbeep();
	}
    }
}

/* User is asked to pick a position on map.  This will iterate until
   '.' designates the final position. */

int
ask_position(prompt, xp, yp)
char *prompt;
int *xp, *yp;
{
    char ch;
    int dir, nx, ny;

    prevmode = mode;
    mode = PROMPTXY;
    sprintf(text1, "%s", prompt);
    sprintf(text2, " [direction keys to move, '.' to set]");
    show_toplines();
    save_cur();
    while (1) {
	ch = wait_for_char();
	if (ch == '.') {
	    *xp = curx;  *yp = cury;
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return TRUE;
	}
	if (ch == ESCAPE) {
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return FALSE;
	}
	if ((dir = iindex(ch, dirchars)) >= 0) {
	    point_in_dir(curx, cury, dir, &nx, &ny);
	    if (inside_area(nx, ny)) {
		curx = nx;  cury = ny;
		put_on_screen(curx, cury);
		show_cursor();
	    } else {
		xbeep();
	    }
	} else {
	    xbeep();
	}
    }
}

/* Save away the currently selected position. */

void
save_cur()
{
    tmpcurx = curx;  tmpcury = cury;
    tmpcurunit = curunit;
}

/* Restore the saved "cur" slots. */

void
restore_cur()
{
    curx = tmpcurx;  cury = tmpcury;
    curunit = tmpcurunit;
}

/* Read a string from the prompt window.  Deletion is allowed, and a
   cursor is displayed. */

int
ask_string(prompt, dflt, strp)
char *prompt, *dflt, **strp;
{
    char ch;
    int done = FALSE, rslt = FALSE;

    sprintf(text1, "%s ", prompt);
    reqstrbeg = strlen(text1);
    /* If a default was supplied, add it. */
    if (dflt != NULL)
      strcat(text1, dflt);
    reqstrend = strlen(text1);
    prevmode = mode;
    mode = PROMPT;
    show_toplines();
    while (!done) {
	ch = wait_for_char();
	switch (ch) {
	  case '\r':
	  case '\n':
	    /* Return a copy of the part of the buffer that was typed into. */
	    if (strp != NULL)
	      *strp = copy_string(text1 + reqstrbeg);
	    done = TRUE;
	    rslt = TRUE;
	    break;
	  case ESCAPE:
	    xbeep();
	    done = TRUE;
	    break;
	  case BACKSPACE:
	  case 0x7f:
	    if (reqstrend > reqstrbeg) {
		--reqstrend;
	    } else {
		xbeep();
	    }
	    break;
	  default:
	    if (reqstrend < BUFSIZE-2) {
		/* Insert the character. */
		(text1)[reqstrend++] = ch;
	    } else {
		xbeep();
	    }
	}
	/* Make sure we're always properly terminated. */
	(text1)[reqstrend] = '\0';
	show_toplines();
    }
    /* We're done, put everything back the way it was. */
    mode = prevmode;
    clear_toplines();
    show_cursor();
    return rslt;
}

/* Ask for a side. */

Side *
ask_side(prompt, dflt)
char *prompt;
Side *dflt;
{
    char ch;
    Side *rslt = dflt;

    prevmode = mode;
    mode = PROMPT;
    sprintf(text1, "%s ", prompt);
    show_toplines();
    while (1) {
	show_cursor();
	ch = wait_for_char();
	if (ch == ESCAPE) {
	    rslt = dflt;
	    break;
	}
	if (between('0', ch, '9')) {
	    rslt = side_n(ch - '0');
	    break;
	}
	if (ch == '\r' || ch == '\n')
	  break;
	xbeep();
    }
    mode = prevmode;
    clear_toplines();
    return rslt;
}

int
ask_unit(prompt, unitp)
char *prompt;
Unit **unitp;
{
    char ch;
    int ndirs, dir, nx, ny;
    Unit *nextocc;

    *unitp = NULL;
    prevmode = mode;
    mode = PROMPTXY;
    sprintf(text1, "%s", prompt);
    sprintf(text2, " [direction keys to move, '.' to set]");
    show_toplines();
    save_cur();
    while (1) {
	ch = wait_for_char();
	if (ch == '.') {
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return TRUE;
	}
	if (ch == ESCAPE) {
	    restore_cur();
	    mode = prevmode;
	    clear_toplines();
	    show_cursor();
	    return FALSE;
	}
	/* Also recognize the "in" ("occupant") command. */
	if (ch == 'i') {
	    if (*unitp != NULL)
	      *unitp = find_next_occupant(*unitp);
	    make_current(*unitp);
	    continue;
	}
	ndirs = char_to_dir(ch, &dir, NULL, NULL);
	if (ndirs > 0) {
	    point_in_dir(curx, cury, dir, &nx, &ny);
	    if (inside_area(nx, ny)) {
		make_current_at(nx, ny);
		if (curunit != NULL)
		  *unitp = curunit;
	    } else {
		xbeep();
	    }
	} else {
	    xbeep();
	}
    }
    return FALSE;
}

/* Given a unit, make it be the one that we are "looking at". */

void
make_current(unit)
Unit *unit;
{
    curunit = unit;
    if (in_play(unit)) {
	curx = unit->x;  cury = unit->y;
    }
    show_closeup();
    show_cursor();
}

/* Give a location, make it be current, and pick a unit there
   to be the current unit. */

void
make_current_at(x, y)
int x, y;
{
    Unit *unit;

    curx = x;  cury = y;
    curunit = NULL;
    /* Look for a top-level unit to become the new current one. */
    for_all_stack(x, y, unit) {
	if (unit->side == dside) {
	    make_current(unit);
	    break;
	}
    }
    put_on_screen(curx, cury);
}

/* Add 'P' for printing/saving to file (which?). */
/* (Need individual screen scrolling also) */

void
interpret_help()
{
    HelpNode *prevhelpnode = cur_help_node;
    extern int first_visible_help_pos, last_visible_help_pos;

    switch (inpch) {
      case ' ':
        first_visible_help_pos = last_visible_help_pos;
        show_help();
	break;
      case 'n':
	cur_help_node = cur_help_node->next;
	break;
      case 'p':
	cur_help_node = cur_help_node->prev;
	break;
      case 'b':
	xbeep();
	break;
      case 't':
	cur_help_node = topics_help_node;
	break;
      case '?':
	cur_help_node = help_help_node;
	break;
      case ESCAPE:
      case 'q':
	mode = prevmode;
	redraw();
	break;
      default:
	xbeep();
	break;
    }
    if (prevhelpnode != cur_help_node) {
        first_visible_help_pos = last_visible_help_pos = 0;
	show_help();
    }
}

static void
describe_help(arg, key, buf)
int arg;
char *key, *buf;
{
    tprintf(buf, "' ' to page down through a node\n");
    tprintf(buf, "'n' to go to next help node\n");
    tprintf(buf, "'p' to go to previous node\n");
    tprintf(buf, "'b' to go back to last node\n");
    tprintf(buf, "'t' to go to topics list\n");
    tprintf(buf, "'?' to go to this help info\n");
    tprintf(buf, "'q' or ESC to go back to game\n");
}

/* Reading is usually pretty fast, so don't do anything here. */

void
announce_read_progress()
{
}

/* Announce the start of a time-consuming computation. */

void
announce_lengthy_process(msg)
char *msg;
{
    n_seconds_elapsed(0);
    announcemsg = copy_string(msg);
    if (announcemsg) {
	printf("%s;", announcemsg);
	fflush(stdout);
	announcemsg = NULL;
	announced = TRUE;
    }
}

void
announce_progress(percentdone)
int percentdone;
{
    if (n_seconds_elapsed(2)) {
	printf(" %d%%,", percentdone);
	fflush(stdout);
	announced = TRUE;
    }
}

void
finish_lengthy_process()
{
    if (announced) {
	printf(" done.\n");
	announced = FALSE;
    }
}

/* This is the basic routine to get out of the program - we assume any
   game hacking has been taken care of already. */

void
exit_cconq()
{
    close_displays();
    /* Leave any commentary on the console. */
    printf("\n%s\n", exit_commentary(dside));
    printf("\n%s\n", get_scores(dside));
    exit(0);
}

void
flush_display_buffers(side)
Side *side;
{
    if (active_display(side)) {
	refresh();
    }
}

/* A predicate that tests whether our display can validly be written to. */

int
active_display(side)
Side *side;
{
    return (side == dside
	    && dside != NULL
	    && side_has_display(dside)
	    && active);
}

/* Draw an individual detailed cell, as a row of one, on all maps. */

void
update_cell_display(side, x, y, rightnow)
Side *side;
int x, y;
int rightnow;
{
    if (active_display(side)) {
	draw_row(x, y, 1);
	if (rightnow)
	  refresh();
    }
}

/* Curses updates efficiently, so don't need special routine. */

void
update_side_display(side, side2, rightnow)
Side *side, *side2;
int rightnow;
{
    if (active_display(side)) {
	show_side_list();
	if (rightnow)
	  refresh();
    }
}

/* Just change the part of the unit list relevant to the given unit. */

void
update_unit_display(side, unit, rightnow)
Side *side;
Unit *unit;
int rightnow;
{
    if (active_display(side) && unit != NULL) {
	if (curunit == unit) {
	    show_closeup();
	    show_map();
	}
	show_side_list();
	show_list();
	if (rightnow)
	  refresh();
    }
}

void
update_unit_acp_display(side, unit, rightnow)
Side *side;
Unit *unit;
int rightnow;
{
    if (active_display(side)) {
    }
}

void
update_turn_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side)) {
	show_game_date();
	if (rightnow)
	  refresh();
	if (g_turn() >= autofinish_start + autofinish_count) {
	    set_autofinish(dside, FALSE);
	}
    }
}

void
update_action_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side)) {
	show_side_list();
	if (rightnow)
	  refresh();
    }
}

void
update_action_result_display(side, unit, rslt, rightnow)
Side *side;
Unit *unit;
int rslt, rightnow;
{
    Action *action;

    if (active_display(side)) {
    	action = (unit->act ? &(unit->act->nextaction) : NULL);
    	if (action == NULL)
    	  return;
    	DGprintf("%s %s result is %s\n",
		 unit_desig(unit), action_desig(action), hevtdefns[rslt].name);
	if (rightnow)
	  refresh();
    }
}

void
update_event_display(side, hevt, rightnow)
Side *side;
HistEvent *hevt;
int rightnow;
{
    /* (should add to some sort of history display) */
}

void
update_fire_at_display(side, unit, unit2, m, rightnow)
Side *side;
Unit *unit, *unit2;
int m, rightnow;
{
    /* (should flash at target) */
}

/* This is for animation of fire-into actions. */

void
update_fire_into_display(side, unit, x, y, z, m, rightnow)
Side *side;
Unit *unit;
int x, y, z, m, rightnow;
{
    /* (should flash at target) */
}

/* Updates to clock need to be sure that display changes immediately. */

void
update_clock_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side)) {
	show_clock();
	show_cursor();
	if (rightnow)
	  refresh();
    }
}

void
update_message_display(side, sender, str, rightnow)
Side *side, *sender;
char *str;
int rightnow;
{
    if (active_display(side)) {
	notify(side, "%s", str);
	if (rightnow)
	  refresh();
    }
}

void
update_all_progress_displays(str, s)
char *str;
int s;
{
}

void
update_everything()
{
    redraw();
}

void
action_point(side, x, y)
Side *side;
int x, y;
{
    if (!active_display(dside))
      return;
    if (!inside_area(x, y))
      return;
    /* (should scroll over to make action point visible, but
        probably not change current point?) */
}

void
close_displays()
{
    if (!active_display(dside))
      return;
    clear();
    refresh();
    endwin();
    active = FALSE;
    DGprintf("Display \"%s\" closed.\n", dside->player->displayname);
}

/* An init error needs to have the command re-run. */

void
low_init_error(str)
char *str;
{
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
}

/* A warning just gets displayed, no other action is taken. */

void
low_init_warning(str)
char *str;
{
    fprintf(stdout, "Warning: %s.\n", str);
    fflush(stdout);
}

/* A run error is fatal. */

void
low_run_error(str)
char *str;
{
    close_displays();
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
    fprintf(stderr, "Saving the game...");
    write_entire_game_state(error_save_filename());
    fprintf(stderr, " done.\n");
    fflush(stderr);
    exit(1);
}

/* Runtime warnings are for when it's important to bug the players,
   usually a problem with Xconq or a game design. */

void
low_run_warning(str)
char *str;
{
    if (active_display(dside)) {
	notify(dside, "Warning: %s; continue? ", str);
	wait_for_char();
    } else {
	low_init_warning(str);
    }
}

void
print_form(form)
Obj *form;
{
	print_form_and_value(stdout, form);
}

void
end_printing_forms()
{
}

/* Even a curses interface can do simple "movies". */

int
#ifdef __STDC__
schedule_movie(Side *side, enum movie_type movie, ...)
#else
schedule_movie(side, movie)
Side *side;
enum movie_type movie;
#endif
{
    return 0;
}

void
play_movies(sidemask)
SideMask sidemask;
{
}

void
ui_save_state(side)
Side *side;
{
  Obj *state = lispnil;

  side->uidata = replace_at_key(side->uidata, "curses", state);
}

void
low_send(id, buf)
int id;
char *buf;
{
}

int
low_receive(id, buf, maxchars, timeout)
int *id, maxchars, timeout;
char *buf;
{
  return 0;
}

void
make_generic_image_data(imf)
ImageFamily *imf;
{
}
