/* A minimal interface to Xconq.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This interface uses only what is
   required by ANSI, so it should run just about everywhere without
   any changes.  This is most useful for testing the kernel and game
   design libraries.

   This file can also serve as a starting point for writing a new interface,
   since it has simple or default implementations of the routines that any
   interface has to support. */

#include "conq.h"
extern void low_send PARAMS ((int id, char *buf));
extern int low_receive PARAMS ((int *id, char *buf, int maxchars,
				int timeout));
#include "print.h"
#include "cmdline.h"

#ifdef COMPILER
extern void compile PARAMS ((void));
#endif /* COMPILER */

static void init_displays PARAMS ((void));
static void get_input PARAMS ((void));
static void interpret_command PARAMS ((Obj *cmd));
static void list_one_unit PARAMS ((Unit *unit));
static void interpret_help PARAMS ((Side *side, char *str));
static int do_cmd PARAMS ((Side *side, Obj *cmd, Obj *parms));
#ifdef DEBUGGING
static void toggle_debug PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void toggle_debugg PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void toggle_debugm PARAMS ((Side *side, Obj *cmd, Obj *parms));
#endif /* DEBUGGING */
static void list_sides PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void list_units PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void list_actors PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void list_cells PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_task_cmd PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_finish_turn PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_finish_all PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_repeat PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_multiple PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_free_run PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_save PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_eval PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_help PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_print PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_memory PARAMS ((Side *side, Obj *cmd, Obj *parms));
static void do_quit PARAMS ((Side *side, Obj *cmd, Obj *parms));
static int do_action PARAMS ((Side *side, Unit *unit, Obj *cmd, Obj *args));
static void show_help PARAMS ((Side *side, HelpNode *node));
static void describe_commands PARAMS ((int, char *, char *));

#ifdef THINK_C
/* This is to get the command line reader in Think C on the Mac. */
#include <console.h>
#endif /* THINK_C */

#ifdef THINK_C
/* Think C loses on the compiler. */
#undef COMPILER
#endif /* THINK_C */

/* This structure maintains state that is local to a side's display.
   At the very least, it must track when the display is open and closed. */

typedef struct a_ui {
    int active;
} UI;

Side *defaultside = NULL;

HelpNode *curhelpnode;

int freerunturns = 0;

int repetition = 0;

Obj *multicmd = NULL;

time_t skelturnstart;

int numcellupdatesperturn = 0;

int numusefulcellupdatesperturn = 0;

/* The main program just calls the setup routines, then enters an infinite
   loop interpreting input and running the simulation. */

int
main(argc, argv)
int argc;
char *argv[];
{
    extern long initrandstate, randstate;
    long currandstate;

#ifdef THINK_C
    /* This is how Think C picks up a command line. */
    argc = ccommand(&argv);
#endif
    printf("Skeleton Xconq version %s\n", version_string());
    printf("(C) %s\n", copyright_string());
    init_library_path(NULL);
    clear_game_modules();
    init_data_structures();
    /* Dump the random state so we can reproduce the run if necessary. */
    printf("Random state is %d", randstate);
    if (initrandstate != randstate) {
	printf(" (seed was %d)", initrandstate);
    }
    printf("\n");
    currandstate = randstate;
    parse_command_line(argc, argv, general_options);
    if (currandstate != randstate) {
	printf("Random state is now %d.\n", randstate);
    }
    load_all_modules();
    /* See if we have something resembling a valid game.  A synth method might
       still change some numbers, but it's up to the method to do it right. */
    check_game_validity();
    parse_command_line(argc, argv, variant_options);
    set_variants_from_options();
    parse_command_line(argc, argv, player_options);
    set_players_from_options();
    parse_command_line(argc, argv, leftover_options);
    make_trial_assignments();
    calculate_globals();
    run_synth_methods();
    final_init();
    assign_players_to_sides();
    init_displays();
    init_signal_handlers();
    run_game(0);
    multicmd = lispnil;
    time(&skelturnstart);
    while (1) {
	if (freerunturns > 0) {
	    if (probability(10)) {
	    	Dprintf("No apparent progress, forcing the turn to finish.\n");
	    	do_finish_all(NULL, lispnil, lispnil);
	    }
	    if (endofgame)
	      freerunturns = 0;
	} else {
	    get_input();
	}
	/* We stay in here until nothing more to do. */
	while (run_game(99) > 0)
	  ;
    }
}

Player *
add_default_player()
{
    Player *dflt = add_player();

    dflt->displayname = "stdio";
    Dprintf("Added the default player %s\n", player_desig(dflt));
    return dflt;
}

/* This routine handles all the displays that might need to be opened. */

static void
init_displays()
{
    Side *side;

    for_all_sides(side) {
	if (side_has_display(side)) {
	    side->ui->active = TRUE;
	    printf("%s now has an open display.\n", side_desig(side));
	}
    }
}

/* Create a user interface, but leave it turned off. */

void
init_ui(side)
Side *side;
{
    if (side_wants_display(side)) {
        side->ui = (UI *) xmalloc(sizeof(UI));
	/* Display should not become active yet. */
	side->ui->active = FALSE;
	defaultside = side;
	set_autofinish(side, FALSE);
	DGprintf("Created a UI for %s\n", side_desig(side));
    } else {
	side->ui = NULL;
    }
}

/* This tests whether the side has a display and if it is in use. */

int
active_display(side)
Side *side;
{
    return (side && side_has_display(side) && side->ui->active);
}

/* Input reading waits for a number of sides, possibly times out. */

static void
get_input()
{
    int cmdlineno = 1, endlineno = 1;
    Obj *cmd;
    Side *side = NULL;

    if (realtime_game()) {
	for_all_sides(side) {
	    update_clock_display(side, TRUE);
	}
    }
    /* should say which sides we're waiting for */
    printf("> ");
    fflush(stdout);
    if (repetition-- > 0) {
	cmd = multicmd;
    } else {
	cmd = read_form(stdin, &cmdlineno, &endlineno);
    }
    if (cmd != lispeof) {
	Dprintlisp(cmd);
	interpret_command(cmd);
    } else {
	printf("EOF reached\n");
	/* should just close one display, leave others running */
	exit(0);
    }
}

Unit *thisunit;

/* Do some simple command parsing, just enough to exercise the program. */

static void
interpret_command(origcmd)
Obj *origcmd;
{
    char *str;
    Obj *cmd, *verb = lispnil, *parms = lispnil;
    Side *side = NULL;
    Unit *unit = NULL;

    thisunit = NULL;
    cmd = origcmd;
    if (consp(cmd) && numberp(car(cmd))) {
	side = side_n(c_number(car(cmd)));
	cmd = cdr(cmd);
    }
    if (consp(cmd) && numberp(car(cmd))) {
	unit = find_unit(c_number(car(cmd)));
	thisunit = unit;
	cmd = cdr(cmd);
    }
    if (consp(cmd)) {
	verb = car(cmd);
	parms = cdr(cmd);
    } else if (symbolp(cmd)) {
	verb = cmd;
    }
    if (verb == lispnil) {
    } else if (do_cmd(side, verb, parms)) {
    } else if (do_action(side, unit, verb, parms)) {
    } else if (symbolp(verb) && *(str = c_string(verb)) == '?') {
	interpret_help(side, str+1);
    } else {
	printf("Command ");
	fprintlisp(stdout, origcmd);
	printf(" not understood, ignoring it\n");
    }
}

/* Random commands. */

static void
toggle_debug(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    if (parms == lispnil) {
	Debug = !Debug;
    } else if (symbolp(car(parms)) && equal(car(parms), intern_symbol("on"))) {
	Debug = TRUE;
    } else {
	Debug = FALSE;
    }
}

static void
toggle_debugm(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    if (parms == lispnil) {
	DebugM = !DebugM;
    } else if (symbolp(car(parms)) && equal(car(parms), intern_symbol("on"))) {
	DebugM = TRUE;
    } else {
	DebugM = FALSE;
    }
}

static void
toggle_debugg(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    if (parms == lispnil) {
	DebugG = !DebugG;
    } else if (symbolp(car(parms)) && equal(car(parms), intern_symbol("on"))) {
	DebugG = TRUE;
    } else {
	DebugG = FALSE;
    }
}

static void
list_sides(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    int u;
    Side *side2;
    Agreement *ag;
    extern int numagreements;

    printf("Sides:\n");
    for_all_sides(side2) {
	printf("%s played by %s\n",
	       side_desig(side2), player_desig(side2->player));
	if (using_tech_levels()) {
	    printf("Tech:");
	    for_all_unit_types(u) {
		if (u_tech_max(u) > 0) {
		    printf("  %s %d/%d",
			   u_type_name(u), side2->tech[u], u_tech_max(u));
		}
	    }
	    printf("\n");
	}
	/* (should say something about mplayer goals here) */
    }
    if (numagreements > 0) {
	printf("Agreements:\n");
	for_all_agreements(ag) {
	    printf("%s\n", agreement_desig(ag));
	}
    }
}

static void
list_units(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    Unit *unit;

    printf("Units:\n");
    for_all_units(unit) {
	list_one_unit(unit);
    }
}

static void
list_actors(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    int i;
    Side *side2;
    Unit *unit;

    printf("Actors (listed by side):\n");
    for_all_sides(side2) {
	printf("%s: %s\n",
	       side_desig(side2),
	       (side2->finishedturn ? "(finished)" : ""));
    }
    for_all_sides_plus_indep(side2) {
	for (i = 0; i < side2->actionvector->numunits; ++i) {
	    unit = (side2->actionvector->units)[i].unit;
	    list_one_unit(unit);
	}
    }
}

static void
list_one_unit(unit)
Unit *unit;
{
    char status[BUFSIZE];

    if (unit == NULL) {
	printf("  -\n");
	return;
    }
    if (!completed(unit)) {
	sprintf(status, " cp %d ", unit->cp);
    } else if (unit->hp < u_hp(unit->type)) {
	sprintf(status, " hp %d ", unit->hp);
    } else {
	sprintf(status, " ");
    }
    printf("  %s%s%s %s\n",
	   unit_desig(unit), status,
	   actorstate_desig(unit->act), plan_desig(unit->plan));
}

static void
list_cells(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    int x = c_number(car(parms)), y = c_number(cadr(parms));

    printf("At %d,%d", x, y);
    if (in_area(x, y)) {
	printf(", terrain %s", t_type_name(terrain_at(x, y)));
	/* (dump borders?) */
	printf(", elev %d", elev_at(x, y));
	printf(", temp %d", temperature_at(x, y));
	/* (etc) */
    } else {
	printf(" - outside area!");
    }
    printf("\n");
}

static void
do_task_cmd(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    int i, j, numargs;
    char *taskname;
    Obj *tasksym = car(parms), *taskparms = cdr(parms);
    Task *task;

    if (symbolp(tasksym)) {
	taskname = c_string(tasksym);
	/* Iterate through task names looking for a match. */
	for (i = 0; taskdefns[i].name != NULL; ++i) {
	    if (strcmp(taskname, taskdefns[i].name) == 0) {
		if (thisunit != NULL && thisunit->plan != NULL) {
		    task = create_task(i);
		    numargs = strlen(taskdefns[i].argtypes);
		    for (j = 0; j < numargs; ++j) {
			if (taskparms != lispnil) {
			    task->args[j] = c_number(car(taskparms));
			    taskparms = cdr(taskparms);
			} else {
			    task->args[j] = 0;
			}
		    }
		    task->next = thisunit->plan->tasks;
		    thisunit->plan->tasks = task;
		    thisunit->plan->waitingfortasks = FALSE;
		}
		return;
	    }
	}
	fprintf(stderr, "Task type \"%s\" not recognized\n", taskname);
    }
}

static void
do_finish_turn(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    if (side) {
	finish_turn(side);
    } else {
	fprintf(stderr, "No side?\n");
    }
}

static void
do_finish_all(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    Side *side2;

    for_all_sides(side2) {
	finish_turn(side2);
    }
}

static void
do_free_run(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    freerunturns = c_number(car(parms));
}

static void
do_repeat(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
}

static void
do_multiple(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    repetition = c_number(car(parms));
    multicmd = cdr(parms);
}

static void
do_save(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    if (!write_entire_game_state(saved_game_filename())) {
	fprintf(stderr, "Save failed.\n");
    }
}

static void
do_eval(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    interp_form(NULL, car(parms));
}

static void
do_help(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    printf("To look at help topics, type \"?<letter>\",\n");
    printf("where 'n' and 'p' go to next and previous nodes\n");
}

static void
do_print(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    char *fname;
    FILE *fp;

    if (parms != lispnil) {
	if (stringp(car(parms))) {
	    fname = c_string(car(parms));
	    if ((fp = fopen(fname, "w")) != NULL) {
		print_game_description_to_file(fp);
		fclose(fp);
	    } else {
		fprintf(stderr, "couldn't open \"%s\"\n", fname);
	    }
	} else {
	    /* error, not a string */
	}
    } else {
	print_game_description_to_file(stdout);
    }
}

static void
do_memory(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    extern int grandtotmalloc;

    printf("%d bytes allocated.\n", grandtotmalloc);
}

/* Exit immediately, no questions asked. */

static void
do_quit(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    printf("Quitting.\n");
    exit(0);
}

#ifdef COMPILER
static void
do_compile(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    compile();
}
#endif /* COMPILER */

struct a_cmd {
    char *cmd;
    void (*fn) PARAMS ((Side *side, Obj *cmd, Obj *parms));
} cmdtable[] = {
    { "debug", toggle_debug },
    { "debugm", toggle_debugm },
    { "debugg", toggle_debugg },
    { "sides", list_sides },
    { "units", list_units },
    { "actors", list_actors },
    { "cells", list_cells },
    { "task", do_task_cmd },
    { "fin", do_finish_turn },
    { "finall", do_finish_all },
    { "again", do_repeat },
    { "*", do_multiple },
    { "run", do_free_run },
    { "save", do_save },
    { "eval", do_eval },
    { "help", do_help },
    { "print", do_print },
    { "memory", do_memory },
    { "quit", do_quit },
#ifdef COMPILER
    { "compile", do_compile },
#endif /* COMPILER */
    { NULL, NULL }
};

/* Try to find and execute an arbitrary command. */

static int
do_cmd(side, cmd, parms)
Side *side;
Obj *cmd, *parms;
{
    struct a_cmd *cmdentry = cmdtable;
    char *cmdstr;

    if (!symbolp(cmd)) return FALSE;
    cmdstr = c_string(cmd);
    while (cmdentry->cmd != NULL) {
	if (strcmp(cmdstr, cmdentry->cmd) == 0) {
	    (*(cmdentry->fn))(side, cmd, parms);
	    return TRUE;
	}
	++cmdentry;
    }
    return FALSE;
}

static int
do_action(side, unit, cmd, args)
Side *side;
Unit *unit;
Obj *cmd, *args;
{
    int randomact = FALSE;
    ActionDefn *actdefn = actiondefns;
    char *cmdstr, *argstr;
    char localbuf[BUFSIZE];
    int i = 0, rslt;
    Obj *rest;
    Action action;

    if (!symbolp(cmd)) return FALSE;
    cmdstr = c_string(cmd);
    if (side == NULL) {
	side = defaultside;
	if (side == NULL) {
	    fprintf(stderr, "Using first side since no defaults avail\n");
	    side = sidelist->next;
	}
    }
    if (unit == NULL) {
	/* (think of something to do here) */
    }
    while (actdefn->name != NULL) {
	if (strcmp(cmdstr, actdefn->name) == 0) {
	    memset(&action, 0, sizeof(Action));
	    action.type = actdefn->typecode;
	    /* Special option to generate random args to action. */
	    if (symbolp(car(args))
		&& strcmp("randomly", c_string(car(args))) == 0) {
		args = cdr(args);
		randomact = TRUE;
	    }
	    if (unit == NULL) {
		if (randomact) {
		    while (!((unit = find_unit(xrandom(numunits))) != NULL
			     && (unit->act != NULL || flip_coin()))
			   && probability(99));
		    if (unit == NULL) {
			fprintf(stderr, "Can't find a unit to act!\n");
			/* We're out of luck, just give up. */
			return TRUE;
		    }
		} else {
		    fprintf(stderr, "No unit to %s!\n", cmdstr);
		    /* *Command* *was* valid, just the args were bad. */
		    return TRUE;
		}
	    }
	    /* Move args from list into action. */
	    argstr = actdefn->argtypes;
	    for (rest = args; rest != lispnil; rest = cdr(rest)) {
		if (argstr[i] != '\0') {
		    action.args[i] = c_number(car(rest));
		} else {
		    break;
		}
		++i;
	    }
	    if (i != strlen(argstr)) {
		if (randomact) {
		    make_plausible_random_args(argstr, i, &(action.args[0]),
					       unit);
		} else {
		    printf("Mismatched args!\n");
		    return TRUE;
		}
	    }
	    sprintf(localbuf, "%s tries %s",
		    unit_desig(unit), action_desig(&action));
	    rslt = execute_action(unit, &action);
	    printf("%s - %s\n", localbuf, hevtdefns[rslt].name);
	    return TRUE;
	}
	++actdefn;
    }
    return FALSE;
}

/* Shut down displays - should be done before any sort of exit. */

void
close_displays()
{
    Side *side;

    for_all_sides(side) {
	if (active_display(side)) {
	    side->ui->active = FALSE;
	    printf("Display \"%s\" closed.\n", side->player->displayname);
	}
    }
}

void
low_notify(side, str)
Side *side;
char *str;
{
    printf("To %s: %s\n", side_desig(side), str);
}

/* (should count # of calls to inactive and non-displayed side...) */

void
update_cell_display(side, x, y, rightnow)
Side *side;
int x, y;
int rightnow;
{
    ++numcellupdatesperturn;
    if (active_display(side)) {
	++numusefulcellupdatesperturn;
	DGprintf("To %s: update view of %d,%d%s\n",
		 side_desig(side), x, y, (rightnow ? " (now)" : ""));
    }
}

/* This hook updates any display of the current turn/date, and any
   other global info, as needed. */

void
update_turn_display(side, rightnow)
Side *side;
int rightnow;
{
    long secs;
    time_t xxx;

    if (active_display(side)) {
	time(&xxx);
	secs = idifftime(xxx, skelturnstart);
	printf("To %s: update to turn %d (%ld seconds since last)",
	       side_desig(side), g_turn(), secs);
	DGprintf("%s", (rightnow ? " (now)" : ""));
	printf("\n");
	printf("%d cell updates, %d useful\n",
	       numcellupdatesperturn, numusefulcellupdatesperturn);
	numcellupdatesperturn = numusefulcellupdatesperturn = 0;
	skelturnstart = xxx;
	--freerunturns;
	if (rightnow) fflush(stdout);
    }
}

void
update_action_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side) && DebugG) {
	printf("To %s: ready to act\n", side_desig(side));
    }
}

void
update_action_result_display(side, unit, rslt, rightnow)
Side *side;
Unit *unit;
int rslt, rightnow;
{
    if (active_display(side) && DebugG) {
	printf("To %s: %s action result is %s\n",
	       side_desig(side), unit_desig(unit), hevtdefns[rslt].name);
    }
}

/* This is for animation of fire-at actions. */

void
update_fire_at_display(side, unit, unit2, m, rightnow)
Side *side;
Unit *unit, *unit2;
int m, rightnow;
{
}

/* This is for animation of fire-into actions. */

void
update_fire_into_display(side, unit, x, y, z, m, rightnow)
Side *side;
Unit *unit;
int x, y, z, m, rightnow;
{
}

void
update_event_display(side, hevt, rightnow)
Side *side;
HistEvent *hevt;
int rightnow;
{
    if (active_display(side)) {
	switch (hevt->type) {
	  case H_SIDE_LOST:
	    printf("%s lost!\n", side_desig(side_n(hevt->data[0])));
	    break;
	  case H_SIDE_WON:
	    printf("%s won!\n",  side_desig(side_n(hevt->data[0])));
	    break;
	  default:
	    DGprintf("To %s: event %s %d\n",
		     side_desig(side), hevtdefns[hevt->type].name,
		     hevt->data[0]);
	}
    }
}

void
update_all_progress_displays(str, s)
char *str;
int s;
{
}

/* This hook should update the side's view of the given side, no matter
   who it belongs to. */

void
update_side_display(side, side2, rightnow)
Side *side, *side2;
int rightnow;
{
    char *side2desc = copy_string(side_desig(side2));

    if (active_display(side) && DebugG) {
	printf("To %s: update side %s%s\n",
	       side_desig(side), side2desc, (rightnow ? " (now)" : ""));
    }
}

/* This hook should update the side's view of the given unit, no matter
   who it belongs to. */

void
update_unit_display(side, unit, rightnow)
Side *side;
Unit *unit;
int rightnow;
{
    if (active_display(side) && DebugG) {
	printf("To %s: update unit %s%s\n",
	       side_desig(side), unit_desig(unit), (rightnow ? " (now)" : ""));
    }
}

void
update_unit_acp_display(side, unit, rightnow)
Side *side;
Unit *unit;
int rightnow;
{
    if (active_display(side) && DebugG) {
	printf("To %s: update unit %s acp%s\n",
	       side_desig(side), unit_desig(unit), (rightnow ? " (now)" : ""));
    }
}

/* This hook updates any realtime clock displays.  If the game does not
   have any realtime constraints, this will never be called. */

void
update_clock_display(side, rightnow)
Side *side;
int rightnow;
{
    if (active_display(side) && DebugG) {
	printf("To %s: %d secs this turn, %d total\n",
	       side_desig(side), side->turntimeused, side->totaltimeused);
	/* also display total game clock */
    }
}

void
update_message_display(side, sender, str, rightnow)
Side *side, *sender;
char *str;
int rightnow;
{
}

void
update_everything()
{
    printf("Update everything!\n");
}

void
action_point(side, x, y)
Side *side;
int x, y;
{
}


/* Generate a description of all the user input that is possible. */

static void
describe_commands(arg, key, buf)
int arg;
char *key, *buf;
{
    struct a_cmd *cmdentry;

    for (cmdentry = cmdtable; cmdentry->cmd != NULL; ++cmdentry) {
	strcat(buf, cmdentry->cmd);
	strcat(buf, "\n");
    }
}

static void
interpret_help(side, topic)
Side *side;
char *topic;
{
    HelpNode *node;

    if (side == NULL) side = defaultside;
    if (side == NULL) {
	fprintf(stderr, "no side to help?\n");
	return;
    }
    if (curhelpnode == NULL) {
	add_help_node("commands", describe_commands, 0, first_help_node);
	curhelpnode = first_help_node;
    }
    switch (topic[0]) {
      case 'n':
	curhelpnode = curhelpnode->next;
	break;
      case 'p':
	curhelpnode = curhelpnode->prev;
	break;
      case 'a':
	for (node = first_help_node->next; node != first_help_node; node = node->next) {
	    show_help(side, node);
	}
	return; /* don't show cur help node too */
      case '\0':
	/* Note that no topic char equals '\0', so will come here. */
      default:
	curhelpnode = first_help_node;
	break;
    }
    show_help(side, curhelpnode);
}

/* Spew out the entire text of the current help node. */

static void
show_help(side, helpnode)
Side *side;
HelpNode *helpnode;
{
    int linelen, skipchar;
    char *linebegin = get_help_text(helpnode), *lineend;

    printf("Topic: %s\n", helpnode->key);
    while (*linebegin != '\0') {
	skipchar = 0;
	lineend = (char *) strchr(linebegin, '\n');
	if (lineend) skipchar = 1;
	linelen = (lineend ? lineend - linebegin : strlen(linebegin));
	if (linelen > 75) linelen = 75;
	strncpy(spbuf, linebegin, linelen);
	spbuf[linelen] = '\0';
	printf("%s\n", spbuf);
	linebegin += linelen + skipchar;
    }
}

/* This reports progress in reading GDL files. */

void
announce_read_progress()
{
}

/* This is used for initialization steps that take a long time. */

int linemiddle = FALSE;

void
announce_lengthy_process(msg)
char *msg;
{
    printf("%s; ", msg);
    fflush(stdout);
    linemiddle = TRUE;
}

/* Update the progress, expressing it as a percentage done. */

void
announce_progress(percentdone)
int percentdone;
{
    printf(" %d%%", percentdone);
    fflush(stdout);
    linemiddle = TRUE;
}

/* Announce the end of the lengthy process. */

void
finish_lengthy_process()
{
    printf(" done.\n");
    linemiddle = FALSE;
}

int
#ifdef __STDC__
schedule_movie(Side *side, enum movie_type movie, ...)
#else
schedule_movie(side, movie)
Side *side;
enum movie_type movie;
#endif
{
    return FALSE;
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
    /* No state to save. */
}

void
flush_display_buffers(side)
Side *side;
{
    if (active_display(side) && DebugG) {
	printf("To %s: flush display buffers\n", side_desig(side));
    }
}

/* An init error needs to have the command re-run. */

void
low_init_error(str)
char *str;
{
    if (linemiddle) printf("\n");
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
}

/* A warning just gets displayed, no other action is taken. */

void
low_init_warning(str)
char *str;
{
    if (linemiddle) printf("\n");
    fprintf(stdout, "Warning: %s.\n", str);
    fflush(stdout);
}

/* A run error is fatal. */

void
low_run_error(str)
char *str;
{
    if (linemiddle) fprintf(stderr, "\n");
    fprintf(stderr, "Error: %s.\n", str);
    fflush(stderr);
    exit(1);
}

/* Runtime warnings are for when it's important to bug the players,
   usually a problem with Xconq or a game design. */

void
low_run_warning(str)
char *str;
{
    if (linemiddle) printf("\n");
    fprintf(stdout, "Warning: %s.\n", str);
    fflush(stdout);
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

#include "imf.h"

void
make_generic_image_data(imf)
ImageFamily *imf;
{
}
