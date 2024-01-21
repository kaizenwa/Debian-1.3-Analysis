/* The main program of the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void notify_instructions PARAMS ((void));
extern void low_send PARAMS ((int id, char *buf));
extern int low_receive PARAMS ((int *id, char *buf, int maxchars,
				int timeout));
#include "cmdline.h"
#include "xconq.h"

/* Local function declarations. */

static void quit_program PARAMS ((Widget wdgt, XEvent *event, String *params,
				  Cardinal *num_params));
static void run_game_proc PARAMS ((XtPointer client_data, XtIntervalId *id));
static Boolean run_game_idle PARAMS ((XtPointer clientdata));
static int handle_x_error PARAMS ((Display *dpy, XErrorEvent *evt));
static int handle_xio_error PARAMS ((Display *dpy));
static void handle_xt_error PARAMS ((String msg));

/* X-specific options, plus some NULL definitions so that they'll be
   ignored by X parsing and get passed through to the generic Xconq
   command-line parser. */

XrmOptionDescRec xoptions[] = {
    { "-background",	"*background",	XrmoptionSepArg,	NULL },
    { "-bg",		"*background",	XrmoptionSepArg,	NULL },
    { "-display",	".display",	XrmoptionSepArg,	NULL },
    { "-f",		NULL,		XrmoptionSkipArg,	NULL },
    { "-fg",		"*foreground",	XrmoptionSepArg,	NULL },
    { "-fn",		"*font",	XrmoptionSepArg,	NULL },
    { "-font",		"*font",	XrmoptionSepArg,	NULL },
    { "-foreground",	"*foreground",	XrmoptionSepArg,	NULL },
    { "-g",		NULL,		XrmoptionSkipArg,	NULL },
    { "-geometry",	"*geometry",	XrmoptionSepArg,	NULL },
    { "-n",		NULL,		XrmoptionNoArg,		NULL },
    { "-name",		".name",	XrmoptionSepArg,	NULL },
    { "-xrm",		NULL,		XrmoptionResArg,	NULL }
};

int xoptions_count;

/* Fallback resources come from an include file that is auto-generated
   from Xconq.ad.  Note that this will only supply a basic b/w layout;
   to do color, the Xconq-co.ad resources have to be loaded already. */

String fallback_resources[] = {

#include "xconqad.h"

  NULL
};

int announced = FALSE;

char *announcemsg = NULL;

int nargs;
Arg tmpargs[100];

XtAppContext thisapp;

Widget thistoplevel;

Widget choiceshell = NULL;

Widget scenarioshell = NULL;

XtActionsRec quit_actions_table[] = {
    { "wm-quit", quit_program },
};

static void
quit_program(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    Side *side;
    Map *map;

    /* The side with toplevel matching widget just quit. */
    if (!find_side_and_map_via_a_toplevel(w, &side, &map))
      return;

    if (w == side->ui->shell) {
	/* Go into the standard quit command. */
	do_quit(side, map);
	/* (should reset prefixarg, or doesn't matter?) */
    } else if (w == side->ui->help_shell) {
	popdown_help(side);
    }
}

/* The main program. */

int
main(argc, argv)
int argc;
char *argv[];
{
    init_library_path(NULL);
    printf("\n              Welcome to X11 Xconq version %s\n\n",
	   version_string());
    printf("%s", license_string());
    print_any_news();
    /* Fiddle with game module structures. */
    clear_game_modules();
#ifdef DEBUGGING
    init_debug_to_stdout();
#endif /* DEBUGGING */
    /* Set up empty data structures. */
    init_data_structures();
    /* Do the usual Xt application setup. */
    /* Note that this opens one display by default, which means that
       later code should take note and not try to open this a second time.
       Also, this will absorb all X-related arguments - anything remaining
       is either a generic Xconq option or a mistake. */
    xoptions_count = XtNumber(xoptions);
    thistoplevel =
      XtAppInitialize(&thisapp, PROGRAMCLASSNAME,
		      xoptions, XtNumber(xoptions), &argc, argv,
		      fallback_resources, (ArgList) NULL, 0);

    parse_command_line(argc, argv, general_options);

    if (option_popup_new_game_dialog) {
	collect_possible_games();
	popup_game_dialog();
    }

    if (option_popup_new_game_dialog) {
	check_player_displays();
    } else {
	load_all_modules();
	check_game_validity();
	parse_command_line(argc, argv, variant_options);
	set_variants_from_options();
	parse_command_line(argc, argv, player_options);
	set_players_from_options();
	make_trial_assignments();
	check_player_displays();
	/* Complain about anything that's left. */
	parse_command_line(argc, argv, leftover_options);
	/* (still need to merge some databases derived from display) */
    }

    /* And, perform some once per application context initialization... */
    XtAppAddActions(thisapp, quit_actions_table, XtNumber(quit_actions_table));
    add_map_actions();
    XtAppAddTimeOut(thisapp, 10, run_game_proc, NULL);
    XtAppAddWorkProc(thisapp, run_game_idle, NULL);

    /* Do the time-consuming part of setup calculations. */
    calculate_globals();
    run_synth_methods();
    final_init();
    assign_players_to_sides();
    print_instructions();
    run_game(0);
    /* Get the displays set up, but don't draw anything yet. */
    init_all_displays();
    /* Now bring up the init data on each display. */
    init_redraws();
    /* Set up the signal handlers. */
    init_signal_handlers();
    init_x_signal_handlers();
    notify_instructions();
    /* Go into the main play loop. */
    XtAppMainLoop(thisapp);

    /* Humor the compiler. */
    return 0;
}

/* This is a timed call; it gets called periodically. */

static void
run_game_proc(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
    int rslt;
    unsigned long interval;

    /* Run the kernel itself. */
    rslt = run_game(10);
    /* Set up to call it again in a little while. */
    /* If things are happening, call 40 times/sec, for responsiveness. */
    interval = 25;
    /* If nothing is happening right now, do at 4 times/sec. */
    if (rslt == 0)
      interval = 250;
    XtAppAddTimeOut(thisapp, interval, run_game_proc, NULL);
}

static Boolean
run_game_idle(clientdata)
XtPointer clientdata;
{
    Side *side;
    Map *map;
    Unit *unit;

    /* See if we should jump to another unit and make it current. */
    for_all_sides(side) {
	if (active_display(side)) {
	    for_all_maps(side, map) {
		if (map->curtool == movetool) {
		    unit = autonext_unit_inbox(side, map->curunit, map->vp);
		    if (unit != NULL)
		      set_current_unit(side, map, unit);
		}
	    }
	}
    }
    /* Call us again! */
    return False;
}

/* The default (human) player is the current user on the current display. */

Player *
add_default_player()
{
    Player *player = add_player();
    
    player->name = getenv("USER");
    player->configname = getenv("XCONQ_CONFIG");
    player->displayname = getenv("DISPLAY");
    return player;
}

/* An init error needs to have the command re-run. */

void
low_init_error(str)
char *str;
{
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
    exit(1);
}

/* A warning just gets displayed, no other action is taken. */

void
low_init_warning(str)
char *str;
{
    fprintf(stderr, "Warning: %s.\n", str);
    fflush(stderr);
}

void
low_run_error(str)
char *str;
{
    close_displays();
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
    fprintf(stderr, "Saving the game...");
    write_entire_game_state(saved_game_filename());
    fprintf(stderr, " done.\n");
    exit(1);
}

/* Runtime warnings are for when it's important to bug the players,
   usually a problem with Xconq or a game design. */

void
low_run_warning(str)
char *str;
{
    notify_all("Warning: %s; continuing...", str);
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

void
init_x_signal_handlers()
{
    XSetErrorHandler(handle_x_error);
    XSetIOErrorHandler(handle_xio_error);
    XtAppSetErrorHandler(thisapp, handle_xt_error);
}

/* Handlers for X catastrophes attempt to do a save first. */

static int
handle_x_error (dpy, evt)
Display *dpy;
XErrorEvent *evt;
{
    static int num_errors = 0;
    char buf[BUFSIZE];

    XGetErrorText(dpy, evt->error_code, buf, BUFSIZE);
    fprintf(stderr, "\nX error on display %s: %s\n", DisplayString(dpy), buf);
    if (++num_errors >= 10) {
        printf("\nX error: trying emergency save!\n");
	/* Note that if the save fails too, we're totally hosed. */
	/* (should use configurable name here) */
        write_entire_game_state("ack!.xconq");
	abort();
    }
    return 0;
}

static int
handle_xio_error (dpy)
Display *dpy;
{
    fprintf(stderr, "\nX IO error on display %s: trying emergency save!\n",
	   DisplayString(dpy));
    write_entire_game_state("ack!.xconq");
    abort();
    return 0;
}

static void
handle_xt_error(msg)
String msg;
{
    fprintf(stderr, "Xt error: %s\n", msg);
    /* Get a core dump to debug with. */
    abort();
}

/* Reading is usually pretty fast, so don't do anything special here. */

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
	announcemsg = NULL;
	fflush(stdout);
	announced = TRUE;
    }
}

/* Announce the making of progress on the computation. */

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

/* Announce the end of the time-consuming computation. */

void
finish_lengthy_process()
{
    if (announced) {
	printf(" done.\n");
	announced = FALSE;
    }
}

/* All update_xxx_display callbacks are here. */

/* Draw an individual detailed hex, as a row of one, on all maps. */

void
update_cell_display(side, x, y, rightnow)
Side *side;
int x, y, rightnow;
{
    Map *map;

    if (active_display(side)) {
	for_all_maps(side, map) {
	    if (rightnow == 36)
	      continue;
	    draw_row(side, map, x, y, 1, TRUE);
	    if (map->curunit && map->curunit->x == x && map->curunit->y == y) {
		draw_current(side, map);
	    }
	}
	if (rightnow)
	  flush_output(side);
    }
}

/* The kernel calls this to update info about the given side. */

void
update_side_display(side, side2, rightnow)
Side *side, *side2;
int rightnow;
{
    Map *map;

    if (active_display(side)) {
	for_all_maps(side, map) {
	    draw_side_info(side, map, side2);
	    /* (is this handled elsewhere also?) */
	    if (endofgame) {
		if (!all_see_all)
		  side->may_set_see_all = TRUE;
		update_controls(side, map);
	    }
	}
	if (rightnow)
	  flush_output(side);
    }
}

/* The kernel calls this to update info about the given unit. */

void
update_unit_display(side, unit, rightnow)
Side *side;
Unit *unit;
int rightnow;
{
    Map *map;

    if (active_display(side) && unit != NULL) {
	if (inside_area(unit->x, unit->y)) {
	    update_cell_display(side, unit->x, unit->y, rightnow);
	}
	/* Redraw any/all info about unit and its side. */
	for_all_maps(side, map) {
	    draw_map_info(side, map);
	    draw_side_info(side, map, unit->side);
	    update_unit_type_list(side, map, unit->type);
	}
	if (rightnow)
	  flush_output(side);
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
update_action_result_display(side, unit, rslt, rightnow)
Side *side;
Unit *unit;
int rslt, rightnow;
{
    Action *action;
    Unit *unit2;
    char *unit2handle = NULL;

    if (active_display(side)) {
	action = &(unit->act->nextaction);
	switch (rslt) {
	  case A_ANY_DONE:
	    /* (anything worthwhile to show?) */
	    break;
	  case A_ANY_TOO_FAR:
	    if (action->type == ACTION_FIRE_AT) {
	      unit2 = find_unit(action->args[0]);
	      if (unit2) {
		unit2handle = unit_handle(side, unit2);
		if (unit2handle)
		  unit2handle = copy_string(unit2handle);
	        notify(side, "Distance to %s (%d) is out of range (%d) of %s.",
			unit2handle ? unit2handle : "target",
			distance(unit->x, unit->y, unit2->x, unit2->y),
			u_range(unit->type),
			unit_handle(side, unit));
		if (unit2handle)
			free(unit2handle);
	      } else {
		notify(side, "That is out of range of your %s.",
			unit_handle(side, unit));
	      }
	    }
	    break;
	  default:
	    break;
	}
	if (Debug) {
	    notify(side, "%s %s %s!", unit_desig(unit),
		   action_desig(action), hevtdefns[rslt].name);
	}
    }
}

/* The kernel calls this to update the global game state. */

void
update_turn_display(side, rightnow)
Side *side;
int rightnow;
{
    int u;
    Map *map;

    if (active_display(side)) {
	for_all_maps(side, map) {
	    draw_game_state(side, map);
	    draw_view_in_panner(side, map);
	    for_all_unit_types(u) {
		update_unit_type_list(side, map, u);
	    }
	}
	if (rightnow)
	  flush_output(side);
    }
}

void
update_action_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side)) {
	/* show state of actions */
	if (rightnow)
	  flush_output(side);
    }
}

void
update_event_display(side, hevt, rightnow)
Side *side;
HistEvent *hevt;
int rightnow;
{
    Unit *unit;
    Side *side2;
    Map *map;

    if (active_display(side)) {
	switch (hevt->type) {
	  case H_SIDE_LOST:
	    if (hevt->data[0] == side_number(side)) {
		notify(side, "You lost!");
	    } else {
		notify(side, "%s lost!", side_desig(side_n(hevt->data[0])));
	    }
	    break;
	  case H_SIDE_WON:
	    if (hevt->data[0] == side_number(side)) {
		notify(side, "You won!");
	    } else {
		notify(side, "%s won!", side_desig(side_n(hevt->data[0])));
	    }
	    break;
	  case H_GAME_ENDED:
	    notify(side, "The game is over!");
	    /* Enable us to see everything on the map accurately. */
	    if (!all_see_all)
	      side->may_set_see_all = TRUE;
	    for_all_maps(side, map) {
		update_controls(side, map);
	    }
	    break;
	  case H_UNIT_COMPLETED:
	    side2 = side_n(hevt->data[0]);
	    unit = find_unit(hevt->data[1]);
	    if (unit != NULL) {
		if (side2 == side) {
		    notify(side, "You completed %s.",
			   unit_handle(side, unit));
		} else {
		    notify(side, "%s completed %s.",
			   side_desig(side2), unit_handle(side2, unit));
		}
	    }
	    break;
	  case H_UNIT_CREATED:
	    side2 = side_n(hevt->data[0]);
	    unit = find_unit(hevt->data[1]);
	    if (unit != NULL) {
		if (side2 == side) {
		    notify(side, "You created %s.",
			   unit_handle(side, unit));
		} else {
		    notify(side, "%s created %s.",
			   side_desig(side2), unit_handle(side2, unit));
		}
	    }
	    break;
	  default:
	    /* No special display desired. */
	    break;
	}
	if (rightnow)
	  flush_output(side);
    }
}

struct fire_state {
    Side *side;
    Map *map;
    int sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2;
    int step;
} firestates[10];

static void
animate_fire_proc(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
    int which = (int) client_data;
    int i, sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2, dx, dy, xx, yy;
    Side *side;
    Map *map;

    side = firestates[which].side;
    map = firestates[which].map;
    sx1 = firestates[which].sx1;  sy1 = firestates[which].sy1;
    sw1 = firestates[which].sw1;  sh1 = firestates[which].sh1;
    sx2 = firestates[which].sx2;  sy2 = firestates[which].sy2;
    sw2 = firestates[which].sw2;  sh2 = firestates[which].sh2;
    XSetFunction(side->ui->dpy, side->ui->gc, GXinvert);
    XSetLineAttributes(side->ui->dpy, side->ui->gc,
		       2, LineSolid, CapButt, JoinMiter); 
    compute_fire_line_segment(sx1 + sw1 / 2, sy1 + sh1 / 2,
			      sx2 + sw2 / 2, sy2 + sh2 / 2,
			      firestates[which].step, 4, &xx, &yy, &dx, &dy);
    /* Draw one segment of a line between the units. */
    if (dx > 0 || dy > 0) {
	XDrawLine(side->ui->dpy, map->viewwin, side->ui->gc,
		  xx, yy, xx + dx, yy + dy);
	flush_output(side);
    }
    XSetFunction(side->ui->dpy, side->ui->gc, GXcopy);
    XSetLineAttributes(side->ui->dpy, side->ui->gc,
		       1, LineSolid, CapButt, JoinMiter); 
    ++(firestates[which].step);
    if (firestates[which].step < 24) {
	XtAppAddTimeOut(thisapp, 150, animate_fire_proc, NULL);
    }
}

void
update_fire_at_display(side, unit, unit2, m, rightnow)
Side *side;
Unit *unit, *unit2;
int m, rightnow;
{
    int i, sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2, dx, dy, xx, yy;
    char *xxx, extrabuf[BUFSIZE];
    Map *map;
    
    if (active_display(side)) {
	for_all_maps(side, map) {
	    x_xform_unit(side, map, unit, &sx1, &sy1, &sw1, &sh1);
	    x_xform_unit(side, map, unit2, &sx2, &sy2, &sw2, &sh2);
	    firestates[0].side = side;
	    firestates[0].map = map;
	    firestates[0].sx1 = sx1;  firestates[0].sy1 = sy1;
	    firestates[0].sw1 = sw1;  firestates[0].sh1 = sh1;
	    firestates[0].sx2 = sx2;  firestates[0].sy2 = sy2;
	    firestates[0].sw2 = sw2;  firestates[0].sh2 = sh2;
	    firestates[0].step = 0;
	    XtAppAddTimeOut(thisapp, 10, animate_fire_proc, NULL);
	}
	xxx = unit_handle(side, unit);
	strcpy(extrabuf, xxx);
	notify(side, "%s fired at %s!", extrabuf, unit_handle(side, unit2));
    }
}

#if 0
	XSetFunction(side->ui->dpy, side->ui->gc, GXinvert);
	XSetLineAttributes(side->ui->dpy, side->ui->gc, 2, LineSolid, CapButt, JoinMiter); 
	i = 0;
	while (i < 12) {  /* should be a timed loop */
	    for_all_maps(side, map) {
		x_xform_unit(side, map, unit, &sx1, &sy1, &sw1, &sh1);
		x_xform_unit(side, map, unit2, &sx2, &sy2, &sw2, &sh2);
		compute_fire_line_segment(sx1 + sw1 / 2, sy1 + sh1 / 2,
					  sx2 + sw2 / 2, sy2 + sh2 / 2,
					  i, 4, &xx, &yy, &dx, &dy);
		/* Draw one segment of a line between the units. */
		if (dx > 0 || dy > 0) {
		    XDrawLine(side->ui->dpy, map->viewwin, side->ui->gc,
			      xx, yy, xx + dx, yy + dy);
		    flush_output(side);
		}
	    }
	    ++i;
	}
	/* (should clean up after drawing) */
	xxx = unit_handle(side, unit);
	strcpy(extrabuf, xxx);
	notify(side, "%s fired at %s!",
	       extrabuf, unit_handle(side, unit2));
	XSetFunction(side->ui->dpy, side->ui->gc, GXcopy);
	XSetLineAttributes(side->ui->dpy, side->ui->gc, 1, LineSolid, CapButt, JoinMiter); 
    }
}
#endif

/* This is for animation of fire-into actions. */

void
update_fire_into_display(side, unit, x, y, z, m, rightnow)
Side *side;
Unit *unit;
int x, y, z, m, rightnow;
{
    int i, sx1, sy1, sw1, sh1, sx2, sy2, sw2, sh2, dx, dy, xx, yy;
    Map *map;
    
    if (active_display(side)) {
	XSetFunction(side->ui->dpy, side->ui->gc, GXinvert);
	XSetLineAttributes(side->ui->dpy, side->ui->gc, 2, LineSolid, CapButt, JoinMiter); 
	i = 0;
	while (i < 12) {  /* should be a timed loop */
	    for_all_maps(side, map) {
		x_xform_unit(side, map, unit, &sx1, &sy1, &sw1, &sh1);
		xform(side, map, x, y, &sx2, &sy2);
		sw2 = map->vp->hw;  sh2 = map->vp->hh;
		compute_fire_line_segment(sx1 + sw1 / 2, sy1 + sh1 / 2,
					  sx2 + sw2 / 2, sy2 + sh2 / 2,
					  i, 4, &xx, &yy, &dx, &dy);
		if (dx > 0 || dy > 0) {
		    XDrawLine(side->ui->dpy, map->viewwin, side->ui->gc,
			      xx, yy, xx + dx, yy + dy);
		    flush_output(side);
		}
	    }
	    ++i;
	}
	/* (should clean up after drawing) */
	notify(side, "%s fired into %d,%d!",
	       unit_handle(side, unit), x, y);
	XSetFunction(side->ui->dpy, side->ui->gc, GXcopy);
	XSetLineAttributes(side->ui->dpy, side->ui->gc, 1, LineSolid, CapButt, JoinMiter); 
    }
}

/* Updates to clock need to be sure that display changes immediately. */

void
update_clock_display(side, rightnow)
Side *side;
int rightnow;
{
    Map *map;

    if (active_display(side)) {
	for_all_maps(side, map) {
	    draw_game_clocks(side, map);
	}
	if (rightnow)
	  flush_output(side);
    }
}

void
update_message_display(side, sender, str, rightnow)
Side *side, *sender;
char *str;
int rightnow;
{
    if (active_display(side)) {
	notify(side, "From %s: \"%s\"",
	       (sender != NULL ? short_side_title(sender) : "<anon>"), str);
	if (rightnow)
	  flush_output(side);
    }
}

void
update_all_progress_displays(str, s)
char *str;
int s;
{
}

void
action_point(side, x, y)
Side *side;
int x, y;
{
    Map *map;

    if (!active_display(side))
      return;
    if (!inside_area(x, y))
      return;

    for_all_maps(side, map) {
	if (map->follow_action && !in_middle(side, map, x, y)) {
	    put_on_screen(side, map, x, y);
	}
    }    
}

void
flush_display_buffers(side)
Side *side;
{
    if (active_display(side)) {
	flush_output(side);
    }
}

void
update_everything()
{
    Side *side;

    for_all_sides(side) {
	if (active_display(side)) {
	    /* (update all maps?) */
	}
    }
}

/* Close all displays that are still open. */

void
close_displays()
{
    Side *side;

    for_all_sides(side) {
	if (active_display(side))
	  close_display(side);
    }
}

/* This routine should be called before any sort of normal exit. */

void
exit_xconq(side)
Side *side;
{
    /* (should this really happen first?) */
    close_displays();
    /* maybe record in scorefile */
    printf("%s\n", exit_commentary(side));
    exit(0);
}

int
#ifdef ANSI_PROTOTYPES
schedule_movie(Side *side, enum movie_type movie, ...)
#else
schedule_movie(side, movie, a1, a2, a3, a4, a5)
Side *side;
enum movie_type movie;
int a1, a2, a3, a4, a5;
#endif
{
    int i;

    if (!active_display(side))
      return FALSE;
    if (side->ui->numscheduled >= 10)
      return FALSE;
    memset(&(side->ui->movies[side->ui->numscheduled]), 0, sizeof(struct a_movie));
    side->ui->movies[side->ui->numscheduled].type = movie;
#ifdef ANSI_PROTOTYPES
    {
	va_list ap;

	va_start(ap, movie);
	for (i = 0; i < 5; ++i)
	  side->ui->movies[side->ui->numscheduled].args[i] = va_arg(ap, int);
	va_end(ap);
    }
#else
    side->ui->movies[side->ui->numscheduled].args[0] = a1;
    side->ui->movies[side->ui->numscheduled].args[1] = a2;
    side->ui->movies[side->ui->numscheduled].args[2] = a3;
    side->ui->movies[side->ui->numscheduled].args[3] = a4;
    side->ui->movies[side->ui->numscheduled].args[4] = a5;
#endif
    ++side->ui->numscheduled;
    return TRUE;
}

void
play_movies(sidemask)
SideMask sidemask;
{
    int j, unitid, sx, sy, sw, sh;
    Map *map;
    Unit *unit;
    Side *side;

    for_all_sides(side) {
	if (side_in_set(side, sidemask) && active_display(side)) {
	    for (j = 0; j < side->ui->numscheduled; ++j) {
		switch (side->ui->movies[j].type) {
		  case movie_null:
		    break;
		  case movie_miss:
		    unitid = side->ui->movies[j].args[0];
		    unit = find_unit(unitid);
		    if (unit == NULL || !in_area(unit->x, unit->y))
		      continue;
		    for_all_maps(side, map) {
			x_xform_unit(side, map, unit, &sx, &sy, &sw, &sh);
			draw_blast_image(side, map, sx, sy, sw, sh, 0);
		    }
		    break;
		  case movie_hit:
		    unitid = side->ui->movies[j].args[0];
		    unit = find_unit(unitid);
		    if (unit == NULL || !in_area(unit->x, unit->y))
		      continue;
		    for_all_maps(side, map) {
			x_xform_unit(side, map, unit, &sx, &sy, &sw, &sh);
			draw_blast_image(side, map, sx, sy, sw, sh, 1);
		    }
		    break;
		  case movie_death:
		    unitid = side->ui->movies[j].args[0];
		    unit = find_unit(unitid);
		    if (unit == NULL || !in_area(unit->x, unit->y))
		      continue;
		    for_all_maps(side, map) {
			x_xform_unit(side, map, unit, &sx, &sy, &sw, &sh);
			draw_blast_image(side, map, sx, sy, sw, sh, 2);
		    }
		    break;
		  case movie_nuke:
		    break;
		  default:
		    break;
		}
	    }
	    side->ui->numscheduled = 0;
	}
    }
}

void
ui_save_state(side)
Side *side;
{
  Obj *state = lispnil;

  side->uidata = replace_at_key(side->uidata, "x11", state);
}

/* Get rid of extra input events, generally because the situation has
   changed drastically and they are no longer of interest.  This routine
   has relatively few valid uses. */

void
flush_input(side)
Side *side;
{
    DGprintf("doing an input flush\n");
    if (active_display(side)) {
	XSync(side->ui->dpy, TRUE);  
    }
}

/* Build a X-compatible widget name, by replacing non-alphanumerics with
   underscores. */

void
build_name(name, first, second)
char *name, *first, *second;
{
    char *ch;

    strcpy(name, first);
    strcat(name, second);
    for (ch = name; *ch != '\0'; ++ch) {
	if (!isalnum(*ch))
	  *ch = '_';
    }
}

#ifdef DEBUGGING

/* Set all the window backgrounds, borders, GCs, etc, usually to reflect
   inversion of foreground and background. */

void
reset_color_state(side)
Side *side;
{
    XGCValues values;
    unsigned int gcmask = GCForeground | GCBackground;

    /* Set the GCs. */
    values.foreground = side->ui->fgcolor;
    values.background = side->ui->bgcolor;
    XChangeGC(side->ui->dpy, side->ui->gc, gcmask, &values);
    XChangeGC(side->ui->dpy, side->ui->textgc, gcmask, &values);
    XChangeGC(side->ui->dpy, side->ui->ltextgc, gcmask, &values);
    XChangeGC(side->ui->dpy, side->ui->unitgc, gcmask, &values);
}

void
reset_window_colors(side, win)
Side *side;
Window win;
{
    XSetWindowBackground(side->ui->dpy, win, side->ui->bgcolor);
    XSetWindowBorder(side->ui->dpy, win, side->ui->fgcolor);
}

#endif /* DEBUGGING */

/* Completely redo all windows, making no assumptions about appearance.
   This is a last-gasp measure, most redrawing should be restricted
   to only the directly affected windows.  Also this shouldn't be
   done without the user's permission, since it will blow away impending
   input. */

void
redraw(side)
Side *side;
{
    draw_all_maps(side);
    /* (should do popups also?) */
    flush_output(side);
    flush_input(side);
}

/* Trivial abstraction - sometimes other routines want to ensure all output
   so far is actually on the screen and not being buffered up somewhere. */

void
flush_output(side) 
Side *side; 
{  
    XFlush(side->ui->dpy);  
}

/* Beep the beeper! */

void
beep(side)
Side *side;
{
    XBell(side->ui->dpy, side->ui->screen);
}

void
low_notify(side, str)
Side *side;
char *str;
{
    Map *map;

    if (!active_display(side))
      return;
    /* Paste into a subwindow of each map window. */
    for_all_maps(side, map) {
	textw_printf(map->history, "%s\n", str);
    }
    DGprintf("To %s: %s\n", side_desig(side), str);
}

/* General text drawer. */

void
draw_text(side, win, x, y, str, color)
Side *side;
Window win;
int x, y, color;
char *str;
{
    XFontStruct *font = side->ui->textfont;
    GC gc = side->ui->textgc;
    Display *dpy = side->ui->dpy;

    y += font->max_bounds.ascent;
    XSetForeground(dpy, gc, color);
    /* If "color" is bg, assume we want reverse video. */
    XSetBackground(dpy, gc, (color == side->ui->bgcolor ? side->ui->fgcolor : side->ui->bgcolor));
    XDrawImageString(dpy, win, gc, x, y, str, strlen(str));
#ifdef STUPIDFLUSH
    XFlush(dpy);
#endif STUPIDFLUSH
}

/* Frequently-called routine to draw text in the foreground color. */

void
draw_fg_text(side, win, x, y, str)
Side *side;
Window win;
int x, y;
char *str;
{
    draw_text(side, win, x, y, str, side->ui->fgcolor);
}

XawTextPosition
widget_text_length(w)
Widget w;
{
    return XawTextSourceScan (XawTextGetSource (w),
			      (XawTextPosition) 0,
 			      XawstAll, XawsdRight, 1, TRUE);
}

#if 0
void
move_caret_to_start(w)
Widget w;
{
    XawTextSetInsertionPoint(w, (XawTextPosition) 0);
}
#endif

void
move_caret_to_end(w)
Widget w;
{
    XawTextPosition pos;

    pos = widget_text_length(w);
    XawTextSetInsertionPoint(w, pos);
}

static int count_lines PARAMS ((Widget w));

static int
count_lines(w)
Widget w;
{
    int	nlines;
    XawTextPosition	pos, len;

    len = widget_text_length(w);

    nlines = 0;
    pos = 0;
    while ((pos = XawTextSourceScan(XawTextGetSource(w),
				    pos, XawstEOL, XawsdRight, 1, TRUE))
	   != XawTextSearchError) {
	nlines++;
	if (pos >= len)
	  break;
    }
    return nlines;
}

static void drop_lines_from_top PARAMS ((Widget w, int num));

static void
drop_lines_from_top(w, num)
Widget w;
int num;
{
    int	nlines;
    XawTextPosition pos, len;
    XawTextBlock tb;
    XawTextEditType et;

    len = widget_text_length(w);

    nlines = num;
    pos = 0;
    while (nlines &&
	   (pos = XawTextSourceScan(XawTextGetSource(w),
				    pos, XawstEOL, XawsdRight, 1, TRUE))
	   != XawTextSearchError) {
	nlines--;
	if (pos >= len)
	  break;
    }
    
    if (pos == XawTextSearchError)
      return;
    
    tb.firstPos = 0;
    tb.length = 0;
    tb.ptr = NULL;
    tb.format = FMT8BIT;

    /* Can we write to it? */
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNeditType, &et);  nargs++;
    XtGetValues(w, tmpargs, nargs);

    if (et == XawtextRead) {
	/* Make it writable. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, XawtextEdit);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }
    
    XawTextReplace(w, (XawTextPosition) 0, pos, &tb);

    if (et == XawtextRead) {
	/* Set it back. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, et);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }

    move_caret_to_end(w);
}

void
#ifdef ANSI_PROTOTYPES
textw_printf(const Widget w, const char *fmt, ...)
#else
textw_printf(w, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
Widget w;
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    XawTextBlock tb;
    XawTextPosition ins_point;
    XawTextEditType et;
    int nlines;
    char buf[BUFSIZ];

#ifdef ANSI_PROTOTYPES
    {
	va_list vl;

	va_start(vl, fmt);
	(void) vsprintf(buf, fmt, vl);
	va_end(vl);
    }
#else
    sprintf(buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    if (w == (Widget) NULL) {
	printf("%s", buf);
	return;
    }

    tb.ptr = buf;
    tb.length = strlen(buf);
    tb.format = FMT8BIT;
    tb.firstPos = 0;

    /* Can we write to it? */
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNeditType, &et);  nargs++;
    XtGetValues(w, tmpargs, nargs);

    if (et == XawtextRead) {
	/* Make it writable. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, XawtextEdit);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }
    move_caret_to_end(w);
    ins_point = widget_text_length(w);
    XawTextReplace(w, ins_point, ins_point, &tb);
    move_caret_to_end(w);
    if (et == XawtextRead) {
	/* Set it back. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, et);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }
    nlines = count_lines(w);
    /* Need to make this a resource. */
#define maxlines 256
    if (nlines > maxlines)
      drop_lines_from_top(w, nlines - maxlines);
#undef maxlines
}

#if 0
void textw_iprintf PARAMS ((const Widget w, const char *fmt, ...));

void
#ifdef ANSI_PROTOTYPES
textw_iprintf(const Widget w, const char *fmt, ...)
#else
textw_iprintf(w, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
Widget w;
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    XawTextBlock tb;
    XawTextPosition	ins_point;
    XawTextEditType	et;
    char buf[BUFSIZ];

#ifdef ANSI_PROTOTYPES
    {
	va_list		vl;

	va_start(vl, fmt);
	(void) vsprintf(buf, fmt, vl);
	va_end(vl);
    }
#else
    sprintf(buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    tb.ptr = buf;
    tb.length = strlen(buf);
    tb.format = FMT8BIT;
    tb.firstPos = 0;

    /* Can we write to it? */
    nargs = 0;
    XtSetArg(tmpargs[nargs], XtNeditType, &et);  nargs++;
    XtGetValues(w, tmpargs, nargs);

    if (et == XawtextRead) {
	/* Make it writable. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, XawtextEdit);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }
    ins_point = XawTextGetInsertionPoint(w);
    XawTextReplace(w, ins_point, ins_point, &tb);
    if (et == XawtextRead) {
	/* Set it back. */
	nargs = 0;
	XtSetArg(tmpargs[nargs], XtNeditType, et);  nargs++;
	XtSetValues(w, tmpargs, nargs);
    }
}
#endif

#if 0
void
EraseTextWidget(w)
Widget w;
{
    XawTextBlock block;

    block.ptr = "";
    block.length = 0;
    block.firstPos = 0;
    block.format = FMT8BIT;

    XawTextReplace(w, 0, widget_text_length(w), &block);
    /* If this fails, too bad. */
    move_caret_to_end(w);
}
#endif

int
find_side_via_widget(w, sidep)
Widget w;
Side **sidep;
{
    Side *side;

    for_all_sides(side) {
	if (active_display(side)) {
	    if (XtDisplay(w) == side->ui->dpy) {
		*sidep = side;
		return TRUE;
	    }
	}
    }
    return FALSE;
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
