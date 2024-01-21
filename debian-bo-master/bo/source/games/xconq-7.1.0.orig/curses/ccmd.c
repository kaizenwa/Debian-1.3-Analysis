/* Commands for the curses interface to Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
#include "cconq.h"
extern int autofinish_start;
extern int autofinish_count;
extern int drawlinear;
extern char linear_char;
extern char bord_char;
extern char conn_char;

/* Command table. */

typedef struct cmdtab {
    char fchar;                 /* character to match against */
    char *name;                 /* Full name of command */
    char *argtypes;
    void (*fn) PARAMS ((void));  /* pointer to command's function */
    char *help;                 /* short documentation string */
} CmdTab;

static void impl_build PARAMS ((Side *side, Unit *unit, int u2, int n));
static void resize_map PARAMS ((int n));
static void execute_named_command PARAMS ((char *cmdstr));
static int execute_command_from_table PARAMS ((CmdTab *cmdtab));
static int execute_named_command_from_table PARAMS ((char *cmdstr,
						    CmdTab *cmdtab));
static void describe_command_table PARAMS ((int arg, char *key, char *buf,
					   CmdTab *cmdtab));
static void cmd_error PARAMS ((char *fmt, ...));

/* Declarations of all the command functions. */

#undef DEF_CMD
#define DEF_CMD(letter,name,args,FN,help) void FN PARAMS ((void));

#include "cmd.def"

#include "ccmd.def"

/* Define a table of generic commands. */

#define C(c) ((c)-0x40)

CmdTab commands[] = {

#undef DEF_CMD
#define DEF_CMD(LETTER,NAME,ARGS,FN,HELP) { LETTER, NAME, ARGS, FN, HELP },

#include "cmd.def"

  { 0, NULL, NULL, NULL, NULL }
};

/* Define a table of curses-specific commands. */

CmdTab ccommands[] = {

#include "ccmd.def"

  { 0, NULL, NULL, NULL, NULL }
};

/* Argument string following a long-name command. */

static char *cmdargstr;

/* Search in command table and execute function if found, complaining if
   the command is not recognized.  Many commands operate on the "current
   unit", and all uniformly error out if there is no current unit, so put
   that test here.  Also fix up the arg if the value passed is one of the
   specially recognized ones. */

void
execute_command()
{
    if (execute_command_from_table(ccommands))
      return;
    if (execute_command_from_table(commands))
      return;
    cmd_error("unknown command character '%c'", inpch);
}

char tmpkey;

static int
execute_command_from_table(cmdtab)
CmdTab *cmdtab;
{
    CmdTab *cmd;
    char ch = inpch;
    void (*fn) PARAMS ((void));
    
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (ch == cmd->fchar) {
	    fn = cmd->fn;
	    if (fn == NULL) {
		run_warning("no command function for %s (0x%x)?",
			    cmd->name, ch);
		return TRUE;
	    }
	    tmpkey = ch;
	    (*fn)();
	    /* Whatever might have happened, we *did* find the command. */
	    return TRUE;
	}
    }
    return FALSE;
}

static void
execute_named_command(cmdstr)
char *cmdstr;
{
    char *cmdname;

    parse_long_name_command(cmdstr, &cmdname, &cmdargstr, copy_string(cmdstr)); 
    if (empty_string(cmdname)) {
	notify(dside, "No command.");
    }
    /* Look for the command name in the curses-specific table. */
    if (execute_named_command_from_table(cmdname, ccommands))
      return;
    /* Try the generic table. */
    if (execute_named_command_from_table(cmdname, commands))
      return;
    cmd_error("unknown command name \"%s\"", cmdstr);
}

static int
execute_named_command_from_table(cmdstr, cmdtab)
char *cmdstr;
CmdTab *cmdtab;
{
    CmdTab *cmd;
    void (*fn) PARAMS ((void));

    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (strcmp(cmdstr, cmd->name) == 0) {
	    if ((fn = cmd->fn) == NULL) {
		run_warning("no command function for %s?", cmd->name);
		return TRUE;
	    }
	    tmpkey = cmd->fchar;
	    (*fn)();
	    /* Whatever might have happened, we *did* find the command. */
	    return TRUE;
	}
    }
    return FALSE;
}

/* Paste one-line descriptions of commands into the supplied buffer. */

void
describe_commands(arg, key, buf)
int arg;
char *key, *buf;
{
    strcat(buf, "To move a unit, use [hlyubn]\n");
    strcat(buf, "[HLYUBN] moves unit repeatedly in that direction\n");
    strcat(buf, "To look at another unit, use survey mode ('z')\n");
    strcat(buf, "and use [hlyubnHLYUBN] to move the cursor\n");
    strcat(buf, "\n");
    strcat(buf, "Generic commands:\n");
    describe_command_table(arg, key, buf, commands);
    strcat(buf, "Cconq-specific commands:\n");
    describe_command_table(arg, key, buf, ccommands);
}

static void
describe_command_table(arg, key, buf, cmdtab)
int arg;
char *key, *buf;
CmdTab *cmdtab;
{
    CmdTab *cmd;

    strcat(buf, "Single-key commands:\n\n");
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	describe_command (cmd->fchar, cmd->name, cmd->help, TRUE, buf);
    }
    strcat(buf, "\nLong name commands:\n\n");
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	describe_command (cmd->fchar, cmd->name, cmd->help, FALSE, buf);
    }
}

/* Use this macro in any command if it requires a current unit. */

#define REQUIRE_UNIT()  \
  if (!in_play(curunit)) {  \
    curunit = NULL;  \
    cmd_error("No current unit to command!");  \
    return;  \
  }

Unit *lastactor = NULL;

Unit *
find_next_and_look()
{
    Unit *nextunit;

    nextunit = find_next_actor(dside, curunit);
    if (nextunit != NULL) {
	make_current(nextunit);
	show_cursor();
    }
    return nextunit;
}

void
do_add_player()
{
    request_additional_side(cmdargstr);
}

void
do_add_terrain()
{
    int u, t, dir;

    REQUIRE_UNIT();
    u = curunit->type;
    if (ask_direction("Add terrain to where?", &dir)) {
	for_all_terrain_types(t) {
	    if (ut_acp_to_add_terrain(u, t) > 0
		&& curunit->act
		&& curunit->act->acp >= ut_acp_to_add_terrain(u, t)) {
		if (0 <= ut_alter_range(curunit->type, t)) {
		    if (prep_add_terrain_action(curunit, curunit,
						curunit->x, curunit->y,
						dir, t))
		      ;
		    else
		      xbeep();
		}
	    }
	}
    }
}

/* Set which AI is to run the side's play. */

void
do_ai_side()
{
    if (side_has_ai(dside)) {
	set_side_ai(dside, NULL);
    } else {
	set_side_ai(dside, "mplayer");
    }
}

void
do_attack()
{
    int x, y;
    Unit *other;

    REQUIRE_UNIT();
    if (ask_position("Attack where?", &x, &y)) {
	for_all_stack(x, y, other) {
	    if (!unit_trusts_unit(curunit, other)) {
		if (valid(check_attack_action(curunit, curunit, other, 100))) {
		    prep_attack_action(curunit, curunit, other, 100);
		    return;
		}
		/* (should try other types of actions?) */
	    }
	}
	cmd_error("Nothing for %s to attack at %d,%d!",
		  unit_handle(dside, curunit), x, y);
    }
}

/* Set the unit to automatic control.  */

void
do_auto()
{
    REQUIRE_UNIT();
    if (curunit->plan) {
	set_unit_ai_control(dside, curunit, !curunit->plan->aicontrol, FALSE);
	/* a hack */
	curunit->plan->waitingfortasks = !curunit->plan->aicontrol;
    }
}

void
do_build()
{
    short possibles[MAXUTYPES];
    int u, u2, numtypes, ufirst;

    REQUIRE_UNIT();
    u = curunit->type;
    if (!can_build(curunit)) {
	cmd_error("%s can't build anything!", unit_handle(dside, curunit));
	return;
    }
#if 0
    if (!u_occproduce(curunit->type) && curunit->transport != NULL) {
	cmd_error("%s can't build anything while inside another unit!",
		  unit_handle(dside, curunit));
	return;
    }
#endif
    numtypes = 0;
    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0 && type_allowed_on_side(u, dside)) {
	    possibles[u2] = TRUE;
	    ++numtypes;
	    ufirst = u2;
	} else {
	    possibles[u2] = FALSE;
	}
    }
    switch (numtypes) {
      case 0:
	cmd_error("Nothing to build!");
	break;
      case 1:
	/* Only one type to build - just do it. */
	impl_build(dside, curunit, ufirst, prefixarg);
	break;
      default:
	/* Player has to choose a type to build. */
	u2 = ask_unit_type("Type to build:", possibles);
	if (u2 != NONUTYPE) {
	    impl_build(dside, curunit, u2, prefixarg);
	} else {
	    /* should clear toplines */
	}
	break;
    }
}

static void
impl_build(side, unit, u2, n)
Side *side;
Unit *unit;
int u2, n;
{
    if (n < 0)
      n = 99;
    notify(side, "%s will build %d %s",
	   unit_handle(side, unit), n, u_type_name(u2));
    push_build_task(unit, u2, n);
}

void
do_clear_plan()
{
    REQUIRE_UNIT();
    if (curunit->plan) {
	set_unit_plan_type(dside, curunit, PLAN_NONE);
    }
}

void
do_copying()
{
    cur_help_node = copying_help_node;
    do_help();
}

void
do_delay()
{
    Unit *oldcurunit = curunit, *nextmover;

    nextmover = find_next_and_look();

    if (nextmover == NULL) {
	cmd_error("no units???");
    } else if (curunit == oldcurunit) {
	notify(dside, "No other units to move!");
    }
}

void
do_detach()
{
    Unit *unit = curunit;

    REQUIRE_UNIT();
    if (!completed(unit)) {
	cmd_error("cannot detach from an incomplete unit");
	return;
    }
    if (valid(check_transfer_part_action(unit, unit, unit->hp / 2, NULL))) {
	prep_transfer_part_action(unit, unit, unit->hp / 2, NULL);
    } else {
	cmd_error("cannot detach right now");
    }
}

void
do_detonate()
{
    Unit *unit = curunit;

    REQUIRE_UNIT();
    prep_detonate_action(unit, unit, unit->x, unit->y, unit->z);
}

/* Supposedly you could only get to these by typing the full command names. */

void
do_dir()
{
    cmd_error("use the single-character commands instead");
}

void
do_dir_multiple()
{
    cmd_error("use the single-character commands instead");
}

/* Get rid of a unit. */

void
do_disband()
{
    int u;

    REQUIRE_UNIT();
    u = curunit->type;
#ifdef DESIGNERS
    if (dside->designer) {
	kill_unit(curunit, -1);
	return;
    }
#endif /* DESIGNERS */
    if (!completed(curunit)) {
	kill_unit(curunit, H_UNIT_DISBANDED);
    } else if (u_hp_per_disband(u) > 0) {
	if (prep_disband_action(curunit, curunit)) {
	    /* (text should come from game design) */
	    notify(dside, "%s will go home.", unit_handle(dside, curunit));
	} else {
	    /* failed, should say something */
	}
    } else {
	cmd_error("You can't just get rid of %s!",
		  unit_handle(dside, curunit));
    }
}

void
do_disembark()
{
    Unit *transport;

    REQUIRE_UNIT();
    transport = curunit->transport;
    if (transport == NULL) {
	cmd_error("Not in a transport");
	return;
    }
    if (!in_play(transport)) {
	cmd_error("Transport is nonsensical?");
	return;
    }
    /* Try moving into the transport's transport, if there is one. */
    if (transport->transport != NULL
        && can_occupy(curunit, transport->transport)) {
	prep_enter_action(curunit, curunit, transport->transport);
	/* (should be able to set up task if can't do action immedly) */
	return;
    }
    /* Try moving into the open in the cell. */
    if (can_occupy_cell(curunit, curunit->x, curunit->y)
	|| can_occupy_conn(curunit, curunit->x, curunit->y, curunit->z)) {
	prep_move_action(curunit, curunit, curunit->x, curunit->y, curunit->z);
	/* (should be able to set up task if can't do action immedly) */
	return;
    }
    cmd_error("Can't disembark here!");
}

/* Determine how far away another point is.  */

void
do_distance()
{
    int x, y;

    if (ask_position("Distance to where?", &x, &y)) {
	notify(dside, "Distance is %d cells.", distance(curx, cury, x, y));
    }
}

void
do_distrust()
{
    Side *side2;

    if (cmdargstr) {
	side2 = parse_side_spec(cmdargstr);
	if (side2 != NULL && side2 != dside) {
	    set_trust(dside, side2, 0);
	    return;
	} 
    }
    cmd_error("no side to distrust");
}

void
do_draw_willingness()
{
    if (prefixarg < 0)
      prefixarg = 1;
    set_willing_to_draw(dside, (prefixarg ? 1 : 0));
}

/* What about trying to embark a unit on itself or on its previous transp? */

void
do_embark()
{
    Unit *transport;

    REQUIRE_UNIT();
    /* (should only ask if multiple transport possibilities) */
    if (ask_unit("Which unit to board?", &transport)) {
	if (!in_play(transport)) {
	    cmd_error("This transport is nonsensical?");
	    return;
	}
	if (!can_occupy(curunit, transport)) {
	    cmd_error("Can't occupy chosen transport");
	    return;
	}
	prep_enter_action(curunit, curunit, transport);
	/* (should be able to set up task if can't do action immedly) */
    } else {
	xbeep();
    }
}

/* Command to end our activity for this turn. */

void
do_end_turn()
{
    finish_turn(dside);
}

/* Command to fire at a specified unit or location. */

void
do_fire()
{
    int x, y;
    Unit *unit2;

    REQUIRE_UNIT();
    sprintf(spbuf, "Fire %s at where?", unit_handle(dside, curunit));
    /* (should have some sort of range feedback) */
    if (ask_position(spbuf, &x, &y)) {
	if (units_visible(dside, x, y)) {
	    for_all_stack(x, y, unit2) {
		if (unit2->side != curunit->side) {
		    prep_fire_at_action(curunit, curunit, unit2, -1);
		    return;
		}
	    }
	}
	/* (should say that nothing is visible and verify firing) */
	if (1) {
	    prep_fire_into_action(curunit, curunit, x, y, 0, -1);
	} else {
	    xbeep();
	}
    }
}

void
do_fire_into()
{
    int x, y;
    Unit *unit2;

    REQUIRE_UNIT();
    sprintf(spbuf, "Fire %s at where?", unit_handle(dside, curunit));
    /* (should have some sort of range feedback) */
    if (ask_position(spbuf, &x, &y)) {
	prep_fire_into_action(curunit, curunit, x, y, 0, -1);
    }
}

/* Toggle the action following flag. */

void
do_follow_action()
{
    follow_action = !follow_action;
    if (follow_action) {
	notify(dside, "Following the action.");
    } else {
	notify(dside, "Not following the action.");
    }
}

void
do_force_global_replan()
{
    force_global_replan(dside);
}

/* Give supplies to a transport.  The argument tells how many to give. */

void
do_give()
{
    int n = prefixarg, something = FALSE, u, m, r, gift, actual;
    Unit *main;

    REQUIRE_UNIT();
    u = curunit->type;
    main = curunit->transport;
    if (main != NULL) {
	spbuf[0] = '\0';
	m = main->type;
	for_all_material_types(r) {
	    gift = (n < 0 ? (um_storage_x(m, r) - main->supply[r]) : n);
	    if (gift > 0) {
		something = TRUE;
		/* Be stingy if we're low */
		if (2 * curunit->supply[r] < um_storage_x(u, r))
		    gift = max(1, gift/2);
		actual = transfer_supply(curunit, main, r, gift);
		sprintf(tmpbuf, " %d %s", actual, m_type_name(r));
		strcat(spbuf, tmpbuf);
	    }
	}
	if (something) {
	    notify(dside, "%s gave%s.", unit_handle(dside, curunit), spbuf);
	} else {
	    notify(dside, "%s gave nothing.", unit_handle(dside, curunit));
	}
    } else {
	cmd_error("Can't transfer supplies here!");
    }
}

/* Give a unit to another side or "to" independence. */

void
do_give_unit()
{
    int u;

    REQUIRE_UNIT();
    u = curunit->type;
#ifdef DESIGNERS
    if (dside->designer) {
	change_unit_side(curunit, side_n(prefixarg), -1, NULL);
	update_unit_display(dside, curunit, TRUE);
	all_see_cell(curunit->x, curunit->y);
	return;
    }
#endif /* DESIGNERS */
    if (1) { /* (should test both temporary and permanent invalidity) */
	prep_change_side_action(curunit, curunit, side_n(prefixarg));
    } else {
	cmd_error("You can't just give away the %s!", u_type_name(u));
    }
}

/* Bring up help info. */

void
do_help()
{
    /* Switch to help mode, saving current mode first. */
    prevmode = mode;
    mode = HELP;
    show_help();
    refresh();
}

/* Send a short message to another side. */

void
do_message()
{
    char *msg;
    Side *side2;
    SideMask sidemask;

    if (prefixarg == 0) {
	/* (should ask who to send to) */
    }
    side2 = side_n(prefixarg);
    if (ask_string("Message:", "", &msg)) {
	if (empty_string(msg) || (prefixarg >= 0 && side2 == NULL)) {
	    notify(dside, "You keep your mouth shut.");
	    sidemask = NOSIDES;
	} else if (prefixarg < 0) {
	    notify(dside, "You made the announcement \"%s\"", msg);
	    sidemask = ALLSIDES;
	} else if (side2 != NULL) {
	    notify(dside, "Your message was sent.");
	    sidemask = add_side_to_set(side2, NOSIDES);
	}
	if (!empty_string(msg) && sidemask != NOSIDES)
	  send_message(dside, sidemask, msg);
    }
}

/* Set unit to move to a given location.  Designers do a teleport. */

void
do_move_to()
{
    int x, y;

    REQUIRE_UNIT();
    sprintf(spbuf, "Move %s to where?", unit_handle(dside, curunit));
    if (ask_position(spbuf, &x, &y)) {
#ifdef DESIGNERS
	if (dside->designer) {
	    designer_teleport(curunit, x, y, NULL);
	    make_current(curunit);
	    return;
	}
#endif /* DESIGNERS */
	set_move_to_task(curunit, x, y);
    }
}

/* Command to name or rename the current unit or a given side. */

void
do_name()
{
    char *newname;

    REQUIRE_UNIT();
    if (ask_string("New name for unit:", curunit->name, &newname)) {
	if (empty_string(newname))
	  newname = NULL;
	set_unit_name(dside, curunit, newname);
    }
}

void
do_occupant()
{
    Unit *nextocc;

    if (curunit == NULL) {
	make_current_at(curx, cury);
    }
    REQUIRE_UNIT();
    nextocc = find_next_occupant(curunit);
    if (nextocc != curunit)
      make_current(nextocc);
}

void
do_other()
{
    char *cmd;

    if (ask_string("Command:", NULL, &cmd)) {
	if (empty_string(cmd)) {
	    cmd_error("No command");
	} else if (strcmp(cmd, "?") == 0) {
	    cur_help_node = commands_help_node;
	    do_help();
	} else {
	    execute_named_command(cmd);
	}
    }
}

void
do_print_view()
{
    dump_text_view(dside, use_both_chars);
}

void
do_produce()
{
    int m, n;
    Unit *unit = curunit;

    REQUIRE_UNIT();
    if (!can_produce(unit)) {
	cmd_error("cannot do active production");
    }
    n = 9999;
    if (prefixarg > 0)
      n = prefixarg;
    /* Find the first produceable type and set up to produce it. */
    for_all_material_types(m) {
	if (um_acp_to_produce(unit->type, m) > 0) {
	    push_produce_task(unit, m, n);
	    return;
	}
    }
}

/* Command to get out of a game, one way or another. */

void
do_quit()
{
    if (endofgame || !dside->ingame) {
	exit_cconq();
	return;
    }
    /* Confirm the saving of any state. */
    if (!gamestatesafe) {
	if (ask_bool("Do you want to save the game?", TRUE)) {
	    if (all_others_willing_to_save(dside)) {
		do_save();
		exit_cconq();
		return;
	    } else {
		set_willing_to_save(dside, TRUE);
		notify(dside, "Other sides not willing to save.");
	    }
	}
    }
    if (all_others_willing_to_quit(dside)) {
	if (ask_bool("Do you really want to declare a draw?", FALSE)) {
	    set_willing_to_draw(dside, TRUE);
	} else {
	    notify(dside, "Not willing to draw.");
	}
	return;
    } else {
	if (ask_bool("You must resign to get out; do you want to resign?", FALSE)) {
	    do_resign();
	} else {
	    notify(dside, "Not resigning.");
	}
    }
}

/* Move the current location as close to the center of the display as
   possible, and redraw everything. */

void
do_recenter()
{
    set_view_focus(mvp, curx, cury);
    center_on_focus(mvp);
    set_map_viewport();
    show_map();
    refresh();
}

/* Redraw everything using the same code as when windows need a redraw. */

void
do_refresh()
{
    redraw();
}

void
do_remove_terrain()
{
    int t, dir;

    REQUIRE_UNIT();
    if (ask_direction("Remove terrain from where?", &dir)) {
      for_all_terrain_types(t) {
	if (ut_acp_to_remove_terrain(curunit->type, t) > 0
	    && curunit->act
	    && curunit->act->acp >= ut_acp_to_remove_terrain(curunit->type, t)) {
	    if (0 <= ut_alter_range(curunit->type, t)) {
		if (prep_remove_terrain_action(curunit, curunit, curunit->x, curunit->y, dir, t))
		  ;
		else
		  xbeep();
	    }
	}
      }
    }
}

void
do_reserve()
{
    REQUIRE_UNIT();
    set_unit_asleep(dside, curunit, TRUE, TRUE);
}

void
do_resign()
{
    Side *side2;

    if (endofgame) {
	cmd_error("Game is already over.");
    } else if (!dside->ingame) {
	cmd_error("You are already out of the game.");
    } else if (ask_bool("Do you really want to resign?", FALSE)) {
	side2 = NULL;
	if (numsides > 2) {
	    side2 = ask_side("Who do you want to inherit?", NULL);
	    if (side2 == dside) {
		cmd_error("You can't inherit your own units! (not giving to anybody)");
		side2 = NULL;
	    }
	}
	resign_game(dside, side2);
    }
}

/* Set unit to return to a good resupply place. */

void
do_return()
{
    REQUIRE_UNIT();
    set_resupply_task(curunit, NONMTYPE);
}

/* Stuff game state into a file.  By default, it goes into the current
   directory.  If building a scenario, we can specify just which parts
   of the game state are to be written. */

void
do_save()
{
    char *rawcontents;
    Module *module;
    Obj *contents;

#ifdef DESIGNERS
    if (dside->designer) {
	if (ask_string("Data to write?", "everything", &rawcontents)) {
	    /* (should be in a designer_create_module?) */
	    /* need to be able to get this name from somewhere */
	    module = create_game_module("random.scn");
	    /* need something better to turn contents into a Lisp object */
	    contents = intern_symbol(rawcontents);
	    /*	interpret_content_spec(module, contents);  */
	    notify(dside, "Module will be written to \"%s\" ...", module->filename);
	    if (write_game_module(module)) {
		notify(dside, "Done writing to \"%s\".", module->filename);
	    } else {
		cmd_error("Can't open file \"%s\"!", module->filename);
	    }
	    return;
	} else {
	    return;
	}
    }
#endif /* DESIGNERS */
    if (0 /* checkpointing not allowed */) {
	if (ask_bool("You really want to save and exit?", FALSE)) {
	    notify(dside, "Game will be saved to \"%s\" ...", saved_game_filename());
	    if (write_entire_game_state(saved_game_filename())) {
		close_displays();
		/* this should be conditional? */
		exit(0);
	    } else {
		cmd_error("Can't open file \"%s\"!", saved_game_filename());
	    }
	}
    } else {
	notify(dside, "Saving...");
	if (write_entire_game_state(saved_game_filename())) {
	    notify(dside, "Game saved.");
	} else {
	    cmd_error("Couldn't save to \"%s\"!", saved_game_filename());
	}	    
    }
}

void
do_set_formation()
{
    Unit *leader;

    REQUIRE_UNIT();
    sprintf(spbuf, "Which unit to follow?");
    if (ask_unit(spbuf, &leader)) {
	if (!in_play(leader)) {
	    cmd_error("No unit to follow!");
	} else if (leader == curunit) {
	    cmd_error("Unit can't follow itself!");
	} else if (leader->side != dside /* or "trusted side"? */) {
	    cmd_error("Can't follow somebody else's unit!");
	} else {
	    set_formation(curunit, leader, curunit->x - leader->x, curunit->y - leader->y, 1, 1);
	}
    }
}

void
do_set_rate()
{
    int slow, fast;
    char *reststr;

    if (cmdargstr) {
	slow = strtol(cmdargstr, &reststr, 10);
	fast = strtol(reststr, &reststr, 10);
	set_play_rate(slow, fast);
    }
}

void
do_sleep()
{
    REQUIRE_UNIT();
    set_unit_asleep(dside, curunit, TRUE, TRUE);
}

void
do_standing_orders()
{
    int rslt;

    if (cmdargstr) {
	rslt = parse_standing_order(dside, cmdargstr);
	if (rslt < 0)
	  xbeep();
    } else
      xbeep();
}

void
do_surrender_to()
{
    cmd_error("can't surrender to anybody yet");
}

/* Command to toggle between interaction modes. */

void
do_survey()
{
    if (mode == MOVE) {
	lastactor = curunit;
	mode = SURVEY;
    } else {
	mode = MOVE;
	/* If we weren't looking at a unit when we switched modes,
	   go back to the last unit that was being moved. */
	if (curunit == NULL && in_play(lastactor)) {
	    make_current(lastactor);
	}
    }
    show_map();
    refresh();
}

/* Take supplies from transport. */

void
do_take()
{
    int m, rslt;
    short amts[MAXMTYPES];

    REQUIRE_UNIT();
    rslt = take_supplies(curunit, NULL, amts);
    if (rslt) {
	spbuf[0] = '\0';
	for_all_material_types(m) {
	    if (amts[m] > 0) {
		sprintf(tmpbuf, " %d %s", amts[m], m_type_name(m));
		strcat(spbuf, tmpbuf);
	    }
	}
	notify(dside, "%s got%s.", unit_handle(dside, curunit), spbuf);
    } else {
	notify(dside, "%s got nothing.", unit_handle(dside, curunit));
    }
}

void
do_take_unit()
{
    cmd_error("can't take units yet");
}

void
do_trust()
{
    Side *side2;

    if (cmdargstr) {
	side2 = parse_side_spec(cmdargstr);
	if (side2 != NULL && side2 != dside) {
	    set_trust(dside, side2, 1);
	    return;
	} 
    }
    cmd_error("no side to trust");
}

/* Wake *everything* (that's ours) within the given radius.  Two commands
   actually; "top-level" units (not in a transport) vs all units. */

/* Wake top-level units. */

void
do_wake()
{
    wake_area(dside, curx, cury, prefixarg, FALSE);
}

/* Wake all units found. */

void
do_wake_all()
{
    wake_area(dside, curx, cury, prefixarg, TRUE);
}

/* Display the program version. */

void
do_version()
{
    notify(dside, "Curses Xconq version %s", version_string());
    notify(dside, "(c) %s", copyright_string());
}

void
do_warranty()
{
    cur_help_node = warranty_help_node;
    do_help();
}

/* Curses-specific commands. */

void
do_c_change_list_view()
{
    if (prefixarg < 0) {
	cycle_list_type();
    } else if (prefixarg == 0) {
	cycle_list_filter();
    } else if (prefixarg == 1) {
	cycle_list_order();
    }
    show_list();
    refresh();
}

/* Commands to change the dividing line between the right-hand and left-hand
   windows of the screen. */

void
do_c_grow_map()
{
    if (prefixarg < 0)
      prefixarg = 5;
    if (lw <= 5 && prefixarg > 0) {
	cmd_error("list side must be at least 5");
	return;
    }
    if (lw - prefixarg < 5)
      prefixarg = lw - 5;
    resize_map(prefixarg);
}

void
do_c_shrink_map()
{
    if (prefixarg < 0)
      prefixarg = 5;
    if (mw <= 10 && prefixarg > 0) {
	cmd_error("map side must be at least 10");
	return;
    }
    if (mw - prefixarg < 10)
      prefixarg = mw - 10;
    resize_map(0 - prefixarg);
}

static void
resize_map(n)
int n;
{
    /* Resize the left-hand-side windows. */
    mw += n;
    closeupwin->w += n;
    mapwin->w += n;
    /* Move and resize the right-hand-side windows. */
    lw -= n;
    sideswin->x += n;
    sideswin->w -= n;
    listwin->x += n;
    listwin->w -= n;
    /* Update the screen to reflect the changes. */
    set_scroll();
    redraw();
}

void
do_c_set_info_lines()
{
    if (prefixarg < 0)
      prefixarg = 5;
    if (prefixarg < 1)
      prefixarg = 1;
    if (prefixarg > 10)
      prefixarg = 10;
    infoh = prefixarg;
    mh = LINES - 2 - infoh - 1;
    closeupwin->h = infoh;
    mapwin->y = 2 + infoh;
    mapwin->h = mh;
    /* Update the screen to reflect the changes. */
    set_scroll();
    redraw();
}

void
do_c_run()
{
    int turns;

    turns = atoi(cmdargstr);
    notify(dside, "Running free for %d turn%s.",
	   turns, (turns == 1 ? "" : "s"));
    if (turns > 0) {
	set_autofinish(dside, TRUE);
	autofinish_start = g_turn();
	autofinish_count = turns;
    }
}

/* Set the display of various kinds of data. */

void
do_c_show()
{
    int value;
    char *str, *str2, tmpbuf[BUFSIZE];

    if (cmdargstr) {
	str = cmdargstr;
	while (*str != '\0') {
	    /* Collect the next whitespace-separated token from the
               arg string. */
	    while (*str != '\0' && *str == ' ')
	      ++str;
	    str2 = tmpbuf;
	    while (*str != '\0' && *str != ' ')
	      *str2++ = *str++;
	    *str2 = '\0';
	    str2 = tmpbuf;
	    value = TRUE;
	    /* See if it is prefixed with a "-" or "no". */
	    if (*str2 == '-') {
		value = FALSE;
		++str2;
	    } else if (*str2 == 'n' && *(str2+1) == 'o') {
		value = FALSE;
		str2 += 2;
	    }
	    if (strcmp(str2, "terrain") == 0 || strcmp(str2, "t") == 0) {
		drawterrain = value;
	    } else if (strcmp(str2, "unit") == 0 || strcmp(str2, "u") == 0) {
		drawunits = value;
	    } else if (strcmp(str2, "name") == 0 || strcmp(str2, "n") == 0) {
		drawnames = value;
	    } else if (strcmp(str2, "people") == 0 || strcmp(str2, "p") == 0) {
		drawpeople = value;
	    } else if (strcmp(str2, "one") == 0 || strcmp(str2, "1") == 0) {
		use_both_chars = !value;
	    } else if (strcmp(str2, "two") == 0 || strcmp(str2, "2") == 0) {
		use_both_chars = value;
	    } else if (strncmp(str2, "lin", 3) == 0) {
		drawlinear = value;
		if (value && str2[3] == '=' && str2[4] != '\0') {
		    linear_char = str2[4];
		}
	    } else {
		cmd_error("\"%s\" not recognized", tmpbuf);
	    }
	}
    } else {
	notify(dside, "Nothing to do.");
    }
    show_map();
    refresh();
}

#ifdef DESIGNERS

static int check_designer_status PARAMS ((char *str));

/* The following commands are only available to designers. */

int curradius = 0;
int curttype = 0;
int curutype = 0;
int curfeature = 0;
int cursidenumber = 1;

static int
check_designer_status(str)
char *str;
{
    if (dside->designer) {
	return TRUE;
    } else {
	cmd_error("You're not a designer, can't %s!", str);
	return FALSE;
    }
}

void
do_design()
{
    if (!dside->designer) {
	become_designer(dside);
    } else {
	become_nondesigner(dside);
    }
}

void
do_gdl()
{
    if (cmdargstr)
      interp_form(NULL, read_form_from_string(cmdargstr, NULL, NULL));
}

void
do_c_set_unit_type()
{
    int u;

    if (!check_designer_status("set unit types to create"))
      return;
    u = ask_unit_type("Type of unit to create:", NULL);
    if (u != NONUTYPE) {
	curutype = u;
	if (prefixarg >= 0)
	  cursidenumber = prefixarg;
	notify(dside, "will now be creating side %d %s units.",
	       cursidenumber, u_type_name(u));
    }
}

void
do_c_add_unit()
{
    Unit *unit;

    if (!check_designer_status("create units"))
      return;
    unit = designer_create_unit(dside, curutype, cursidenumber, curx, cury);
    if (unit != NULL) {
	make_current(unit);
    } else {
	cmd_error("Unit creation failed!");
    }
}

void
do_c_set_terrain_type()
{
    int t;

    if (!check_designer_status("set ttypes to paint"))
      return;
    t = ask_terrain_type("Type of terrain:", NULL);
    if (t != NONTTYPE) {
	curttype = t;
	if (prefixarg >= 0)
	  curradius = prefixarg;
	notify(dside, "will now be painting %d-radius %s.",
	       curradius, t_type_name(t));
    }
}

/* Terrain painting command. */

void
do_c_paint_terrain()
{
    int t;

    /* (should ask for dir for linear types?) */
    if (!check_designer_status("paint terrain"))
      return;
    /* If command's arg is nonegative, interpret as temporary
       change of terrain type. */
    t = (prefixarg >= 0 ? prefixarg : curttype);
    paint_cell(dside, curx, cury, curradius, t);
}

/* (should add painting for all other layers here) */

#endif /* DESIGNERS */

#ifdef DEBUGGING

void
do_debug()
{
#ifndef Debug
    toggle_debugging(&Debug);
#endif
}

void
do_debugg()
{
#ifndef DebugG
    toggle_debugging(&DebugG);
#endif
}

void
do_debugm()
{
#ifndef DebugM
    toggle_debugging(&DebugM);
#endif
}

#endif /* DEBUGGING */

/* Generic command error routine just does a notify. */

static void
#ifdef __STDC__
cmd_error(char *fmt, ...)
#else
cmd_error(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char tmpnbuf[BUFSIZE];

#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, fmt);
	vsprintf(tmpnbuf, fmt, ap);
	va_end(ap);
    }
#else
    sprintf(tmpnbuf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_notify(dside, tmpnbuf);
    xbeep();
}
