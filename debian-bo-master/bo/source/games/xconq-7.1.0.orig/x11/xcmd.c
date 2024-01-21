/* Commands for the X11 interface to Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "xconq.h"
extern void create_orders_window PARAMS ((Side *side, Map *map));

char *cmdargstr = NULL;

/* The command table is an array of all the commands. */

typedef struct cmdtab {
    char fchar;			/* character to match against */
    char *name;			/* Full name of command */
    char *argtypes;		/* String giving info about arguments */
    void (*fn) PARAMS ((Side *side, Map *map));  /* pointer to command's function */
    char *help;                 /* short documentation string */
} CmdTab;

static int execute_command_from_table PARAMS ((Side *side, Map *map,
					       CmdTab *cmdtab));
static void execute_named_command PARAMS ((Side *side, Map *map,
					   char *cmdstr));
static int execute_named_command_from_table PARAMS ((Side *side, Map *map,
						    char *cmdstr,
						    CmdTab *cmdtab));
static void describe_commands PARAMS ((int arg, char *key, char *buf));
static void describe_command_table PARAMS ((int arg, char *key, char *buf,
					   CmdTab *cmdtab));
static void aux_add_terrain PARAMS ((Side *side, Map *map, int cancel));
static void aux_add_terrain_2 PARAMS ((Side *side, Map *map, int cancel));
static void do_add_terrain_2 PARAMS ((Side *side, Map *map, int t));
static void aux_build PARAMS ((Side *side, Map *map, int cancel));
static void impl_build PARAMS ((Side *side, Unit *unit, int u2, int n));
static void aux_distance PARAMS ((Side *side, Map *map, int cancel));
static void aux_fire_at PARAMS ((Side *side, Map *map, int cancel));
static void aux_move_look PARAMS ((Side *side, Map *map));
static void aux_move_dir PARAMS ((Side *side, Map *map, Unit *unit));
static void aux_message PARAMS ((Side *side, Map *map, int cancel));
static void aux_move_to PARAMS ((Side *side, Map *map, int cancel));
static void aux_name PARAMS ((Side *side, Map *map, int cancel));
static void aux_others PARAMS ((Side *side, Map *map, int cancel));
static void aux_resign PARAMS ((Side *side, Map *map, int cancel));
static void aux_resign_2 PARAMS ((Side *side, Map *map, int cancel));
static void aux_leave_game PARAMS ((Side *side, Map *map, int cancel));
static void aux_kill_game PARAMS ((Side *side, Map *map, int cancel));
static void aux_resign_b PARAMS ((Side *side, Map *map, int cancel));
static void aux_save_2 PARAMS ((Side *side, Map *map, int cancel));
static void aux_save_1 PARAMS ((Side *side, Map *map, int cancel));
static void aux_save_1_1 PARAMS ((Side *side, Map *map, int cancel));
static void aux_set_formation PARAMS ((Side *side, Map *map, int cancel));
static void save_the_game PARAMS ((Side *side));
static void cmd_error PARAMS ((Side *side, char *fmt, ...));

char tmpkey;

Map *tmpmap;

#define C(c) ((c)-0x40)

#undef DEF_CMD
#define DEF_CMD(LETTER,NAME,ARGS,FN,HELP) { LETTER, NAME, ARGS, FN, HELP },

/* Define a table of generic Xconq commands. */

CmdTab commands[] = {

#include "cmd.def"

  { 0, NULL, NULL, NULL, NULL }
};

/* Define a table of additional X-specific commands. */

CmdTab xcommands[] = {

#include "xcmd.def"

  { 0, NULL, NULL, NULL, NULL }
};

static HelpNode *commands_help_node;

/* Search in command table and execute function if found, complaining if
   the command is not recognized.  Many commands operate on the "current
   unit", and all uniformly error out if there is no current unit, so put
   that test here. */

void
execute_command(side, map)
Side *side;
Map *map;
{
    int found;

    side->ui->beepcount = 0;
    cmdargstr = NULL;
    /* Look through the X-specific command table. */
    found = execute_command_from_table(side, map, xcommands);
    if (found)
      return;
    /* Try the generic table. */
    found = execute_command_from_table(side, map, commands);
    if (found)
      return;
    cmd_error(side, "unrecognized command key '%c'", map->inpch);
}

static int
execute_command_from_table(side, map, cmdtab)
Side *side;
Map *map;
CmdTab *cmdtab;
{
    CmdTab *cmd;
    char ch = map->inpch;
    void (*fn) PARAMS ((Side *side, Map *map));
    
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (ch == cmd->fchar) {
	    fn = cmd->fn;
	    if (fn == NULL) {
		run_warning("no command function for %s (0x%x)?",
			    cmd->name, ch);
		return TRUE;
	    }
	    tmpkey = ch;
	    (*fn)(side, map);
	    /* Whatever might have happened, we *did* find the command. */
	    return TRUE;
	}
    }
    return FALSE;
}

static void
execute_named_command(side, map, cmdstr)
Side *side;
Map *map;
char *cmdstr;
{
    int found;
    char *cmdname;

    parse_long_name_command(cmdstr, &cmdname, &cmdargstr, copy_string(cmdstr)); 
    if (empty_string(cmdname)) {
	notify(side, "No command.");
    }
    /* Look for the command name in the X-specific table. */
    found = execute_named_command_from_table(side, map, cmdname, xcommands);
    if (found)
      return;
    /* Try the generic table. */
    found = execute_named_command_from_table(side, map, cmdname, commands);
    if (found)
      return;
    cmd_error(side, "unrecognized command name \"%s\"", cmdname);
}

static int
execute_named_command_from_table(side, map, cmdstr, cmdtab)
Side *side;
Map *map;
char *cmdstr;
CmdTab *cmdtab;
{
    CmdTab *cmd;
    void (*fn) PARAMS ((Side *side, Map *map));

    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (strcmp(cmdstr, cmd->name) == 0) {
	    fn = cmd->fn;
	    if (fn == NULL) {
		run_warning("no command function for %s?", cmd->name);
		return TRUE;
	    }
	    tmpkey = cmd->fchar;
	    (*fn)(side, map);
	    /* Whatever might have happened, we *did* find the command. */
	    return TRUE;
	}
    }
    return FALSE;
}

/* Describe all the commands. */

static void
describe_commands(arg, key, buf)
int arg;
char *key, *buf;
{
    describe_command_table(arg, key, buf, commands);
    strcat(buf, "\n");
    describe_command_table(arg, key, buf, xcommands);
}

/* Describe all the commands to be found in a given table. */

static void
describe_command_table(arg, key, buf, cmdtab)
int arg;
char *key, *buf;
CmdTab *cmdtab;
{
    CmdTab *cmd;

    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (cmd->fchar != '\0') {
	    if (cmd->fchar < ' ' || cmd->fchar > '~') { 
		tprintf(buf, "^%c", (cmd->fchar ^ 0x40));
	    } else {
		tprintf(buf, " %c", cmd->fchar);
	    }
	    strcat(buf, " ");
	    strcat(buf, cmd->help);
	    strcat(buf, "\n");
	}
    }
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
	if (cmd->fchar == '\0') {
	    strcat(buf, "\"");
	    strcat(buf, cmd->name);
	    strcat(buf, "\"");
	    strcat(buf, " ");
	    strcat(buf, cmd->help);
	    strcat(buf, "\n");
	}
    }
}

/* Use this macro in any command if it requires a current unit. */

#define REQUIRE_UNIT(side, map)  \
  if (!in_play((map)->curunit)) {  \
    (map)->curunit = NULL;  \
    cmd_error((side), "No current unit to command!");  \
    return;  \
  }

/* Definitions of all the command functions. */

void
do_add_player(side, map)
Side *side;
Map *map;
{
    request_additional_side(cmdargstr);
}

void
do_add_terrain(side, map)
Side *side;
Map *map;
{
    int u, t, numtypes, tfirst, possibles[MAXTTYPES];
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    u = unit->type;
    numtypes = 0;
    for_all_terrain_types(t) {
	if (ut_acp_to_add_terrain(u, t) > 0) {
	    possibles[t] = TRUE;
	    ++numtypes;
	    tfirst = t;
	} else {
	    possibles[t] = FALSE;
	}
    }
    if (numtypes == 0) {
	cmd_error(side, "Cannot add terrain using this unit");
    } else if (numtypes == 1) {
	map->argunitid = unit->id;
	do_add_terrain_2(side, map, tfirst);
    } else {
	map->argunitid = unit->id;
	ask_terrain_type(side, map, "Type to add:", possibles, aux_add_terrain);
    }
}

static void
aux_add_terrain(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int t;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Type a key to select terrain type.");
	map->modalhandler = aux_add_terrain;
	return;
    }
    if (grok_terrain_type(side, map, &t)) {
	if (t != NONTTYPE) {
	    do_add_terrain_2(side, map, t);
	}
    } else {
	/* beep? */
    }
}

/* This is like do_add_terrain, but with a terrain type given. */

static void
do_add_terrain_2(side, map, t)
Side *side;
Map *map;
int t;
{
    map->tmpt = t;
    save_cur(side, map);
    ask_position(side, map, "Add/alter where? [click to set]",
		 aux_add_terrain_2);
}

static void
aux_add_terrain_2(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int x, y, dir;
    Unit *unit;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Just pick a location!");
	map->modalhandler = aux_add_terrain_2;
	return;
    }
    if (grok_position(side, map, &x, &y)) {
	if (in_area(x, y)) {
	    unit = find_unit(map->argunitid);
	    if (in_play(unit) && side_controls_unit(side, unit)) {
		switch (t_subtype(map->tmpt)) {
		  case cellsubtype:
		    prep_alter_cell_action(unit, unit, x, y, map->tmpt);
		    break;
		  case bordersubtype:
		  case connectionsubtype:
		    dir = closest_dir(x - unit->x, y - unit->y);
		    prep_add_terrain_action(unit, unit, unit->x, unit->y, dir, map->tmpt);
		    break;
		  case coatingsubtype:
		    cmd_error(side, "can't add coatings yet");
		    break;
		}
	    }
	}
	restore_cur(side, map);
    } else {
	map->modalhandler = aux_add_terrain_2;
	/* beep? */
    }
}

void
do_ai_side(side, map)
Side *side;
Map *map;
{
    if (side_has_ai(side)) {
	set_side_ai(side, NULL);
    } else {
	set_side_ai(side, "mplayer");
    }
}

void
do_attack(side, map)
Side *side;
Map *map;
{
    notify(side, "can't do generic attacks yet");
}

void
do_auto(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;
    
    REQUIRE_UNIT(side, map);
    if (unit->plan) {
	set_unit_ai_control(side, unit, !unit->plan->aicontrol, FALSE);
	/* a hack */
	unit->plan->waitingfortasks = !unit->plan->aicontrol;
    }
}

void
do_build(side, map)
Side *side;
Map *map;
{
    int u, u2, possibles[MAXUTYPES], numtypes, ufirst;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    u = unit->type;
    if (!can_build(unit)) {
	cmd_error(side, "%s can't build anything!", unit_handle(side, unit));
	return;
    }
    if (unit->transport != NULL
	&& !uu_occ_can_build(unit->transport->type, u)) {
	cmd_error(side, "%s can't build anything while inside another unit!",
		  unit_handle(side, unit));
	return;
    }
    numtypes = 0;
    for_all_unit_types(u2) {
	if (uu_acp_to_create(u, u2) > 0 && type_allowed_on_side(u, side)) {
	    possibles[u2] = TRUE;
	    ++numtypes;
	    ufirst = u2;
	} else {
	    possibles[u2] = FALSE;
	}
    }
    switch (numtypes) {
      case 0:
	cmd_error(side, "Nothing to build!");
	break;
      case 1:
	/* Only one type to build - do it. */
	impl_build(side, unit, ufirst, map->prefixarg);
	break;
      default:
	/* Player has to choose a type to build. */
	map->argunitid = unit->id;
	ask_unit_type(side, map, "Type to build:", possibles, aux_build);
	break;
    }
}

static void
aux_build(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int u2;
    Unit *unit;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Type a key or click on a unit type to select build.");
	map->modalhandler = aux_build;
	return;
    }
    if (grok_unit_type(side, map, &u2)) {
	if (u2 != NONUTYPE) {
	    unit = find_unit(map->argunitid);
	    if (in_play(unit) && side_controls_unit(side, unit)) {
		impl_build(side, unit, u2, map->prefixarg);
	    }
	}
    } else {
	/* beep? */
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
do_clear_plan(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    set_unit_plan_type(side, unit, PLAN_NONE);
}

void
do_copying(side, map)
Side *side;
Map *map;
{
    notify(side, "You may copy freely.  See the file COPYING.");
}

void
do_delay(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;

    if (unit != NULL) {
	if (unit->plan)
	  delay_unit(unit, TRUE);
    } else {
	unit = find_next_awake_mover(side, map->curunit);
	if (unit != map->curunit) {
	    set_current_unit(side, map, unit);
	} else {
	    cmd_error(side, "No next awake mover found.");
	}
    }
}

void
do_detach(side, map)
Side *side;
Map *map;
{
    int rslt;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    if (!completed(unit)) {
	cmd_error(side, "%s is incomplete; cannot detach", unit_handle(side, unit));
	return;
    }
    rslt = check_transfer_part_action(unit, unit, unit->hp / 2, NULL);
    if (valid(rslt)) {
	prep_transfer_part_action(unit, unit, unit->hp / 2, NULL);
    } else {
	notify(side, "can't detach for some reason?");
    }
}

void
do_detonate(side, map)
Side *side;
Map *map;
{
    int rslt;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    rslt = check_detonate_action(unit, unit, unit->x, unit->y, unit->z);
    if (valid(rslt)) {
	prep_detonate_action(unit, unit, unit->x, unit->y, unit->z);
    } else {
	notify(side, "can't detonate for some reason?");
    }
}

void
do_dir(side, map)
Side *side;
Map *map;
{
    switch (map->curtool) {
      case looktool:
	if (map->prefixarg < 0)
	  map->prefixarg = 1;
	aux_move_look(side, map);
	break;
      case movetool:
      case unitmovetool:
	if (map->curunit != NULL) {
	    aux_move_dir(side, map, map->curunit);
	} else {
	    cmd_error(side, "No current unit to move!");
	}
	break;
    }
}

void
do_dir_multiple(side, map)
Side *side;
Map *map;
{

    switch (map->curtool) {
      case looktool:
	if (map->prefixarg < 0)
	  map->prefixarg = 10;
	aux_move_look(side, map);
	break;
      case movetool:
      case unitmovetool:
	map->prefixarg = 9999;
	if (map->curunit != NULL) {
	    aux_move_dir(side, map, map->curunit);
	} else {
	    cmd_error(side, "No current unit to move!");
	}
	break;
    }
}

static void
aux_move_look(side, map)
Side *side;
Map *map;
{
    int ndirs, dir, nx, ny;
    Unit *unit;

    ndirs = char_to_dir(tmpkey, &dir, NULL, NULL);
    if (ndirs < 1) {
	cmd_error(side, "what direction is that?!?");
	return;
    }
    if (!point_in_dir_n(map->curx, map->cury, dir, map->prefixarg, &nx, &ny)) {
	beep(side);
	return;
    }
    /* (should share with move_look) */
    clear_current(side, map);
    unit = unit_at(nx, ny);
    if (unit != NULL 
	&& (side_controls_unit(side, unit) || all_see_all)) {
	set_current_unit(side, map, unit);
    } else {
	set_current_xy(side, map, nx, ny);
    }
}
  
static void
aux_move_dir(side, map, unit)
Side *side;
Map *map;
Unit *unit;
{
    int ndirs, dir, n = map->prefixarg, x, y;
  
    if (!unit->act || !unit->plan) { /* use a more sophisticated test? */
	/* ??? can't act ??? */
	return;
    }
    ndirs = char_to_dir(tmpkey, &dir, NULL, NULL);
    if (ndirs < 1) {
	cmd_error(side, "what direction is that?!?");
	return;
    }
    if (n > 1) {
	DGprintf("Ordering %s to move %d %s\n",
		 unit_desig(unit), n, dirnames[dir]);
	set_move_dir_task(unit, dir, n);
    } else {
	if (!point_in_dir(unit->x, unit->y, dir, &x, &y)) {
	    return;
	}
	if (!advance_into_cell(side, unit, x, y, unit_at(x, y))) {
	    beep(side);
	}
	if (in_play(unit)) {
	    map->curx = unit->x;  map->cury = unit->y;
	}
#if 0	/* (this probably should not be here, but check behavior) */
	/* make sure we don't wander too close to the edge... */
	x = map->curx - map->vx;  y = map->cury - map->vy;
	if (y < 3 || y > map->vh - 3) {
	    recenter(side, map, map->curx, map->cury);
	}
#endif
    }
}

/* Get rid of a unit. */

void
do_disband(side, map)
Side *side;
Map *map;
{
    int u;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    u = unit->type;
#ifdef DESIGNERS
    /* A designer can take out any unit, no questions asked. */
    if (side->designer) {
	kill_unit(unit, -1);
	return;
    }
#endif /* DESIGNERS */
    if (!completed(unit)) {
	kill_unit(unit, H_UNIT_DISBANDED);
    } else if (u_hp_per_disband(u) > 0) {
	if (prep_disband_action(unit, unit)) {
	    notify(side, "%s will go home.", unit_handle(side, unit));
	} else {
	    cmd_error(side, "odd failure");
	}
    } else {
	cmd_error(side, "You can't just get rid of the %s!", u_type_name(u));
    }
}

void
do_disembark(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit, *transport;

    REQUIRE_UNIT(side, map);
    transport = unit->transport;
    if (transport == NULL) {
	cmd_error(side, "Not in a transport");
	return;
    }
    if (!in_play(transport)) {
	cmd_error(side, "Transport is nonsensical?");
	return;
    }
    /* Try moving into the transport's transport, if there is one. */
    if (transport->transport != NULL
        && can_occupy(unit, transport->transport)) {
	prep_enter_action(unit, unit, transport->transport);
	/* (should be able to set up task if can't do action immedly) */
	return;
    }
    /* Try moving into the open in the cell. */
    if (can_occupy_cell(unit, unit->x, unit->y)
	|| can_occupy_conn(unit, unit->x, unit->y, unit->z)) {
	prep_move_action(unit, unit, unit->x, unit->y, unit->z);
	/* (should be able to set up task if can't do action immedly) */
	return;
    }
    cmd_error(side, "Can't disembark here!");
}

void
do_distance(side, map)
Side *side;
Map *map;
{
    save_cur(side, map);
    ask_position(side, map, "Distance to where? [click to set]", aux_distance);
}

static void
aux_distance(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int x, y, dist;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Not being helpful yet.");
	map->modalhandler = aux_distance;
	return;
    }
    if (grok_position(side, map, &x, &y)) {
	if (in_area(x, y)) {
	    dist = distance(map->savedcurx, map->savedcury, x, y);
	    notify(side, "The distance is %d cells.", dist);
	}
	restore_cur(side, map);
    } else {
	map->modalhandler = aux_distance;
	/* beep? */
    }
}

void
do_distrust(side, map)
Side *side;
Map *map;
{
    Side *side2;

    if (cmdargstr) {
	side2 = parse_side_spec(cmdargstr);
	if (side2 != NULL && side2 != side) {
	    set_trust(side, side2, 0);
	    /* publicize? */
	    return;
	} 
    }
}

void
do_draw_willingness(side, map)
Side *side;
Map *map;
{
    if (map->prefixarg < 0)
      map->prefixarg = 1;
    set_willing_to_draw(side, (map->prefixarg ? 1 : 0));
}

void
do_embark(side, map)
Side *side;
Map *map;
{
    Unit *transport, *occ;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    /* look for the first possible transport */
    for_all_stack(unit->x, unit->y, transport) {
	/* make sure its not the transport we're in and we can enter it */
	if (transport != unit->transport &&
	    valid(check_enter_action(unit, unit, transport))) {
	    prep_enter_action(unit, unit, transport);
	    return;
	}

	/* check the occupants too */
	for_all_occupants(transport, occ) {
	    if (occ != unit->transport &&
		valid(check_enter_action(unit, unit, occ))) {
		prep_enter_action(unit, unit, occ);
		return;
	    }
	}
    }
    cmd_error(side, "Nothing for this unit to enter!");
}

void
do_end_turn(side, map)
Side *side;
Map *map;
{
    finish_turn(side);
}

void
do_fire(side, map)
Side *side;
Map *map;
{
    int sx, sy, x, y;
    Map *map2;
    Unit *other;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);

    if (map->frombutton) {
	map->argunitid = unit->id;
	save_cur(side, map);
	ask_position(side, map, "Fire at what unit? [click to set]",
		     aux_fire_at);
	return;
    }

    map2 = side->ui->mapdown;
    sx = side->ui->sxdown;  sy = side->ui->sydown;

    if (x_nearest_cell(side, map2, sx, sy, &x, &y)) {
	if (x != unit->x || y != unit->y) {
	    if (unit->act && unit->plan) { /* (should be more sophisticated?) */
		other = unit_at(x, y);
		if (other != NULL) {
		    /* There's a unit to fire at. */
		    if (other->side == unit->side) {
			cmd_error(side, "You can't fire at one of your own units!");
		    } else if (valid(check_fire_at_action(unit, unit, other, -1))) {
			prep_fire_at_action(unit, unit, other, -1);
		    }
		} else {
		    cmd_error(side, "Nothing there to fire at.");
		}
	    }
	}
    }
}

static void
aux_fire_at(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int x, y;
    Unit *unit, *other;

    if (cancel)
      return;
    if (grok_position(side, map, &x, &y)) {
	unit = find_unit(map->argunitid);
	if (x != unit->x || y != unit->y) {
	    if (unit->act && unit->plan) { /* (should be more sophisticated?) */
		other = unit_at(x, y);
		if (other != NULL) {
		    /* There's a unit to fire at. */
		    if (other->side == unit->side) {
			cmd_error(side, "You can't fire at one of your own units!");
		    } else if (valid(check_fire_at_action(unit, unit, other, -1))) {
			prep_fire_at_action(unit, unit, other, -1);
		    }
		} else {
		    cmd_error(side, "Nothing there to fire at");
		}
	    }
	}
	restore_cur(side, map);
    } else {
	map->modalhandler = aux_move_to;
    }
}

void
do_fire_into(side, map)
Side *side;
Map *map;
{
    int sx, sy, x, y, rslt;
    Map *map2;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);

    sx = side->ui->sxdown;
    sy = side->ui->sydown;
    map2 = side->ui->mapdown;

    if (x_nearest_cell(side, map2, sx, sy, &x, &y)) {
	if (x != unit->x || y != unit->y) {
	    rslt = check_fire_into_action(unit, unit, x, y, 0, -1);
	    if (valid(rslt)) {
		prep_fire_into_action(unit, unit, x, y, 0, -1);
	    } else {
		cmd_error(side, "%s fire into %d,%d not valid: %s",
			  unit_handle(side, unit), x, y,
			  action_result_desc(rslt));
	    }
	}
    }
}

/* Toggle the "follow-action" flag. */

void
do_follow_action(side, map)
Side *side;
Map *map;
{
    side->ui->follow_action = !side->ui->follow_action;
    if (side->ui->follow_action) {
	notify(side, "Following the action.");
    } else {
	notify(side, "Not following the action.");
    }
}

void
do_force_global_replan(side, map)
Side *side;
Map *map;
{
    force_global_replan(side);
}

void
do_give(side, map)
Side *side;
Map *map;
{
    int something = FALSE;
    int n, u, m, r, gift, actual;
    Unit *unit = map->curunit, *main = NULL;
    char buf[BUFSIZE];

    if (nummtypes == 0) {
	cmd_error(side, "No materials in this game!");
	return;
    }
    REQUIRE_UNIT(side, map);
    u = unit->type;
    main = unit->transport;
    if (main == NULL) {
	cmd_error(side, "Nothing to give to here!");
	return;
    }
    m = main->type;
    n = (map->prefixarg < 0 ? 1 : map->prefixarg);

    buf[0] = '\0';
    for_all_material_types(r) {
	gift = (n < 0 ? (um_storage_x(m, r) - main->supply[r]) : n);
	if (gift > 0) {
	    something = TRUE;
	    /* Be stingy if we're low */
	    if (2 * unit->supply[r] < um_storage_x(u, r))
	      gift = max(1, gift/2);
	    actual = transfer_supply(unit, main, r, gift);
	    tprintf(buf, " %d %s", actual, m_type_name(r));
	}
    }
    if (something) {
	notify(side, "%s gave%s.", unit_handle(side, unit), buf);
    } else {
	notify(side, "%s gave nothing.", unit_handle(side, unit));
    }
}

/* Give a unit to another side or make it independent. */

/* (but giving to indep should be tested, otherwise might kill unit) */

void
do_give_unit(side, map)
Side *side;
Map *map;
{
    int u;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    u = unit->type;
    if (/* u_change_side(u) || */ side->designer) {
/*	unit_changes_side(unit, side_n(n), CAPTURE, PRISONER);  */
	all_see_cell(unit->x, unit->y);
    } else {
	cmd_error(side, "You can't just give away %s!",
		  unit_handle(side, unit));
    }
}

void
do_help(side, map)
Side *side;
Map *map;
{
    /* Compose the help node for commands and make it be the first one. */
    if (side->ui->curhelpnode == NULL) {
	commands_help_node = add_help_node("commands", describe_commands, 0, first_help_node);
	side->ui->curhelpnode = first_help_node;
    }
    popup_help(side);
}

/* Send a short (1 line) message to another player.  Some messages are
   recognized specially, causing various actions. */

void
do_message(side, map)
Side *side;
Map *map;
{
    char prompt[BUFSIZE];
    Side *side2;

    side2 = side_n(map->prefixarg);
    if (side == side2) {
	cmd_error(side, "You mumble to yourself.");
	return;
    }
    if (side2) {
	sprintf(prompt, "Say to %s: ", short_side_title(side2));
    } else {
	sprintf(prompt, "Broadcast to all: ");
    }
    map->argside = side2;
    ask_string(side, map, prompt, NULL, aux_message);
}

static void
aux_message(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    char *msg;
    SideMask sidemask;

    if (cancel)
      return;
    if (grok_string(side, map, &msg)) {
	if (empty_string(msg)) {
	    notify(side, "You keep your mouth shut.");
	    sidemask = NOSIDES;
	} else if (map->argside == NULL) {
	    notify(side, "You broadcast to everybody.", msg);
	    sidemask = ALLSIDES;
	} else {
	    notify(side, "You send your message.");
	    sidemask = add_side_to_set(map->argside, NOSIDES);
	}
	if (!empty_string(msg) && sidemask != NOSIDES)
	  send_message(side, sidemask, msg);
    } else {
	map->modalhandler = aux_message;
    }
}

/* Set unit to move to a given location.  */

/* The command proper. */

void
do_move_to(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    map->argunitid = unit->id;
    save_cur(side, map);
    ask_position(side, map, "Move to where? [click to set]",
		 aux_move_to);
}

static void
aux_move_to(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int x, y;
    Unit *unit;

    if (cancel)
      return;
    if (grok_position(side, map, &x, &y)) {
	unit = find_unit(map->argunitid);
#ifdef DESIGNERS
	if (side->designer) {
	    designer_teleport(unit, x, y, NULL);
	} else
#endif /* DESIGNERS */
	if (in_play(unit)) {
	    set_move_to_task(unit, x, y);
	} else {
	    beep(side);
	}
	restore_cur(side, map);
    } else {
	map->modalhandler = aux_move_to;
    }
}

/* Name/rename the current unit. */

void
do_name(side, map)
Side *side;
Map *map;
{
    char tmpbuf[BUFSIZE];
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    map->argunitid = unit->id;
    sprintf(tmpbuf, "New name for %s:", unit_handle(side, unit));
    ask_string(side, map, tmpbuf, unit->name, aux_name);
}

static void
aux_name(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    char *name;
    Unit *unit;

    if (cancel)
      return;
    if (grok_string(side, map, &name)) {
	unit = find_unit(map->argunitid);
	if (in_play(unit) && side_controls_unit(side, unit)) {
	    set_unit_name(side, unit, name);
	} else {
	    cmd_error(side, "Nothing here that could be named!");
	}
    } else {
	map->modalhandler = aux_name;
    }
}

/* This is a command to examine all occupants and suboccupants, in an
   inorder fashion. */

/* Should have an option to open up a list window that shows everything
   all at once. */

void
do_occupant(side, map)
Side *side;
Map *map;
{
    Unit *nextocc;
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    nextocc = find_next_occupant(unit);
    if (nextocc != unit) {
	set_current_unit(side, map, nextocc);
    }
}

void
do_other(side, map)
Side *side;
Map *map;
{
    ask_string(side, map, "Command:", "", aux_others);
}

static void
aux_others(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    char *cmd;

    if (cancel)
      return;
    if (grok_string(side, map, &cmd)) {
	if (empty_string(cmd)) {
	    notify(side, "No command.");
	} else if (strcmp(cmd, "?") == 0) {
	    do_help(side, map);
	    /* (should do with special jump routine) */
	    side->ui->curhelpnode = commands_help_node;
	    update_help(side);
	} else {
	    execute_named_command(side, map, cmd);
	}
    } else {
	map->modalhandler = aux_others;
    }
}

void
do_print_view(side, map)
Side *side;
Map *map;
{  
    double conv;

    if (side->ui->ps_pp == NULL)
      side->ui->ps_pp = (PrintParameters *) xmalloc(sizeof(PrintParameters));

    init_ps_print(side->ui->ps_pp);

    /* convert to cm or in */
    if (side->ui->ps_pp->cm) {
	conv = 72 / 2.54;
    } else {
	conv = 72;
    }
    side->ui->ps_pp->cell_size /= conv;
    side->ui->ps_pp->cell_grid_width /= conv;
    side->ui->ps_pp->border_width /= conv;
    side->ui->ps_pp->connection_width /= conv;
    side->ui->ps_pp->page_width /= conv;
    side->ui->ps_pp->page_height /= conv;
    side->ui->ps_pp->top_margin /= conv;
    side->ui->ps_pp->bottom_margin /= conv;
    side->ui->ps_pp->left_margin /= conv;
    side->ui->ps_pp->right_margin /= conv;

    popup_print_setup_dialog(side);
}

void
do_produce(side, map)
Side *side;
Map *map;
{
    notify(side, "can't produce materials by action yet");
}

void
do_quit(side, map)
Side *side;
Map *map;
{
    if (side->ingame) {
	if (all_others_willing_to_quit(side)) {
	    /* Everbody else is willing to get out, but confirm us anyway. */
	    ask_bool(side, map, "Do you really want to quit?", FALSE,
		     aux_kill_game);
	} else {
	    if (1 /* outcome needs resolution */) {
		/* if not everybody willing to draw, then we have to resign */
		ask_bool(side, map, "Do you really want to give up?", FALSE,
			 aux_resign);
	    } else {
		/* Everybody is just participating. */
		ask_bool(side, map, "Do you want to leave this game?", FALSE,
			 aux_leave_game);
	    }
	}
    } else {
	/* We're already out of the game, not really anything to confirm. */
	/* (is this common to all interfaces?) */
	if (all_others_willing_to_quit(side)) {
	    exit_xconq(side);
	} else {
	    close_display(side);
	    if (num_active_displays() == 0) {
		exit_xconq(side);
	    }
	}
    }
}

static void
aux_resign(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	if (numsides > 2) {
	    ask_side(side, map, "Who do you want to surrender to?", NULL,
		     aux_resign_2);
	} else {
	    resign_game(side, NULL);
	}
    }
}

static void
aux_resign_2(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    Side *side2;

    if (cancel)
      return;
    grok_side(side, map, &side2);
    resign_game(side, side2);
}

/* Do the act of leaving the game. */

static void
aux_leave_game(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	remove_side_from_game(side);
	/* Now go back and see what happens if we're not in the game. */ 
	do_quit(side, NULL);
    } else {
	/* nothing to do if we said not to exit */
    }
}

/* (Have an extra confirm for designers not to lose unsaved work?) */

static void
aux_kill_game(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	exit_xconq(side);
    } else {
	/* Nothing to do if we said not to exit. */
    }
}

/* Center the screen on the current location. */

void
do_recenter(side, map)
Side *side;
Map *map;
{
    recenter(side, map, map->curx, map->cury);
}

/* Redraw everything using the same code as when windows need a redraw. */

void
do_refresh(side, map)
Side *side;
Map *map;
{
    redraw(side);
    draw_view_in_panner(side, map);
}

void
do_remove_terrain(side, map)
Side *side;
Map *map;
{
    REQUIRE_UNIT(side, map);
    notify(side, "can't remove terrain yet");
}

void
do_reserve(side, map)
Side *side;
Map *map;
{
    REQUIRE_UNIT(side, map);
    set_unit_reserve(side, map->curunit, TRUE, TRUE /* is this right??? */);
}

/* (should reindent) */
void
do_resign(side, map)
Side *side;
Map *map;
{
	if (endofgame) {
		notify(side, "Game is already over.");
		beep(side);
	} else if (!side->ingame) {
		notify(side, "You are already out of the game.");
		beep(side);
	} else {
		ask_bool(side, map, "You really want to resign?", FALSE, aux_resign_b);
	}
}

/* This is semi-redundant with aux_resign. */

static void
aux_resign_b(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	resign_game(side, NULL);
    }
}

/* Set up a task to resupply the unit. */

/* (should warn if task is likely to fail) */

void
do_return(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    /* (should doublecheck range and error out if no chance) */
    if (in_play(unit)) {
	set_resupply_task(unit, NONMTYPE);
    }
}

/* Stuff game state into a file.  By default, it goes into the current
   directory.  If designing a game, we can specify exactly which parts
   of the game state are to be written. */

/* The command proper just sets up different modal handlers, depending on
   whether we're building (and therefore saving a scenario/fragment), or
   saving as much game state as possible, for resumption later. */

void
do_save(side, map)
Side *side;
Map *map;
{
    Side *side2;

    /* First check to see if anybody is in the middle of doing something
       (like renaming a unit) whose state would be lost. */
    for_all_sides(side2) {
	if (side2 != side && side2->busy) {
	    cmd_error(side, "The %s are busy, can't save right now.",
		      plural_form(side2->name));
	}
    }
#ifdef DESIGNERS
    if (side->designer) {
	ask_string(side, map, "Types of data to save?", "all",
		   aux_save_1);
	return;
    }
#endif /* DESIGNERS */
    if (0 /* (should be "savemustquit") */) {
	ask_bool(side, map, "You really want to save?", FALSE, aux_save_2);
    } else {
	save_the_game(side);
    }
}

/* Make a module appropriate to a save file, write the file, and leave. */

static void
aux_save_2(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	save_the_game(side);
    }
}

static void
save_the_game(side)
Side *side;
{
    char *savename = saved_game_filename();

    notify_all("Game will be saved to \"%s\" ...", savename);
    if (write_entire_game_state(savename)) {
	close_displays();
	/* this should be conditional? */
	exit(0);
    } else {
	cmd_error(side, "Can't open file \"%s\"!", savename);
    }
}


#ifdef DESIGNERS

static Module *tmpmodule;

static void
aux_save_1_1(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    char *filenamespec;

    if (cancel)
      return;
    if (grok_string(side, map, &filenamespec)) {
	tmpmodule->filename = filenamespec;
	notify(side, "File will be written to \"%s\" ...",
	       tmpmodule->filename);
	if (write_game_module(tmpmodule)) {
	    notify(side, "Done writing to \"%s\".", tmpmodule->filename);
	} else {
	    cmd_error(side, "Can't open file \"%s\"!", tmpmodule->filename);
	}
    } else {
	map->modalhandler = aux_save_1_1;
    }
}

static void
aux_save_1(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int save;
    char *contentspec;

    if (cancel)
      return;
    if (grok_string(side, map, &contentspec)) {
	tmpmodule = create_game_module("game-data");
	/* Use the spec string to decide which pieces to save. */
	save = FALSE;
	if (strcmp(contentspec, "all") == 0) {
	    tmpmodule->def_all = TRUE;
	    tmpmodule->compress_layers = TRUE;
	    save = TRUE;
	} else if (strcmp(contentspec, "world") == 0) {
	    tmpmodule->def_world = TRUE;
	    tmpmodule->def_areas = TRUE;
	    tmpmodule->def_area_terrain = TRUE;
	    tmpmodule->def_area_misc = TRUE;
	    tmpmodule->def_area_weather = TRUE;
	    tmpmodule->def_area_material = TRUE;
	    save = TRUE;
	} else {
	    cmd_error(side, "Don't understand content spec \"%s\"!",
		      contentspec);
	    return;
	}
	if (save) {
	    ask_string(side, map, "Save data to where?", "game-data.g",
		       aux_save_1_1);
	} else {
	    notify(side, "Nothing requested to be saved.");
	}
    } else {
	map->modalhandler = aux_save_1;
    }
}

#endif /* DESIGNERS */

void
do_set_formation(side, map)
Side *side;
Map *map;
{
    REQUIRE_UNIT(side, map);
    map->argunitid = map->curunit->id;
    save_cur(side, map);
    ask_position(side, map, "Form up on who? [click to set]",
		 aux_set_formation);
}

static void
aux_set_formation(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int x, y;
    Unit *unit, *leader;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Click on a unit that you want to follow.");
	map->modalhandler = aux_set_formation;
	return;
    }
    if (grok_position(side, map, &x, &y)) {
	unit = find_unit(map->argunitid);
	if (in_play(unit) && in_area(x, y)) {
	    leader = unit_at(x, y);
	    if (in_play(leader)
		&& leader != unit
		&& trusted_side(unit->side, leader->side)) {
		set_formation(unit, leader, leader->x - unit->x, leader->y - unit->y, 1, 1);
	    } else {
		cmd_error(side, "Nobody here to form on!");
	    }
	}
	restore_cur(side, map);
    } else {
	map->modalhandler = aux_set_formation;
	/* beep? */
    }
}

void
do_set_rate(side, map)
Side *side;
Map *map;
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
do_sleep(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;

    REQUIRE_UNIT(side, map);
    set_unit_asleep(side, unit, TRUE, TRUE /* is this right??? */);
}

void
do_standing_orders(side, map)
Side *side;
Map *map;
{
    int rslt;

    if (cmdargstr) {
	rslt = parse_standing_order(side, cmdargstr);
	if (rslt < 0)
	  cmd_error(side, "Parse error");

	/* destroy orders window and structures */
	side->ui->orderlist = NULL;
	if (side->ui->orders_shell) {
	    XtPopdown(side->ui->orders_shell);
	    XtDestroyWidget(side->ui->orders_shell);
	    side->ui->orders_shell = NULL;
	}

    } else
      cmd_error(side, "No arguments given.");
}

void
do_surrender_to(side, map)
Side *side;
Map *map;
{
    notify(side, "can't surrender to a side yet");
}

void
do_survey(side, map)
Side *side;
Map *map;
{
    if (map->curtool == looktool) {
	map->curtool = movetool;
    } else if (map->curtool == movetool) {
	map->curtool = looktool;
    } else {
	beep(side);
	return;
    }
    set_tool_cursor(side, map);
    update_controls(side, map);
}

/* Take supplies from transport. */

void
do_take(side, map)
Side *side;
Map *map;
{
    int something = FALSE;
    int u, u2, m, need, actual, n;
    Unit *unit = map->curunit, *unit2;
    char buf[BUFSIZE];

    if (nummtypes == 0) {
	cmd_error(side, "No materials in this game!");
	return;
    }
    REQUIRE_UNIT(side, map);
    u = unit->type;
    unit2 = unit->transport;
    if (unit2 == NULL) {
	cmd_error(side, "Nothing to take from here!");
	return;
    }
    u2 = unit2->type;
    n = (map->prefixarg < 0 ? 1 : map->prefixarg);

    buf[0] = '\0';
    for_all_material_types(m) {
	need = (n < 0 ? um_storage_x(u, m) - unit->supply[m] : n);
	if (need > 0) {
	    something = TRUE;
	    /* Be stingy if we're low */
	    if (2 * unit2->supply[m] < um_storage_x(u2, m))
	      need = max(1, need/2);
	    actual = transfer_supply(unit2, unit, m, need);
	    tprintf(buf, " %d %s", actual, m_type_name(m));
	}
    }
    if (something) {
	notify(side, "%s got%s.", unit_handle(side, unit), buf);
    } else {
	notify(side, "%s needed nothing.", unit_handle(side, unit));
    }
}

void
do_take_unit(side, map)
Side *side;
Map *map;
{
    REQUIRE_UNIT(side, map);
    notify(side, "can't take units yet");
}

void
do_trust(side, map)
Side *side;
Map *map;
{
    Side *side2;

    if (cmdargstr) {
	side2 = parse_side_spec(cmdargstr);
	if (side2 != NULL && side2 != side) {
	    set_trust(side, side2, 1);
	    return;
	} 
    }
}

/* Command to display the program version. */

void
do_version(side, map)
Side *side;
Map *map;
{
    notify(side, "Xconq version %s", version_string());
    notify(side, "(c) %s", copyright_string());
}

void
do_wake(side, map)
Side *side;
Map *map;
{
    /* Do the curunit explicitly, might only be an occupant. */
    if (in_play(map->curunit)) {
	wake_unit(map->curunit, FALSE, 0, NULL);
	draw_map_info(side, map);
    }
    /* If an argument was given, apply to all "top-level" units
       within the radius specified by the argument. */
    if (map->prefixarg >= 0)
      wake_area(side, map->curx, map->cury, map->prefixarg, FALSE);
}

void
do_wake_all(side, map)
Side *side;
Map *map;
{
    int n;

    n = (map->prefixarg < 0 ? 0 : map->prefixarg);
    wake_area(side, map->curx, map->cury, n, TRUE);
}

void
do_warranty(side, map)
Side *side;
Map *map;
{
    notify(side, "There is no warranty.");
}

/* X-interface-specific commands. */

void
do_x_flash(side, map)
Side *side;
Map *map;
{
    int i;

    if (in_area(map->curx, map->cury)) {
	for (i = 0; i < 2; ++i) {
	    invert_unit_subarea(side, map, map->curx, map->cury);
	    /* (should wait a moment) */
	    invert_unit_subarea(side, map, map->curx, map->cury);
	    draw_row(side, map, map->curx, map->cury, 1, TRUE);
	    if (map->curunit) {
		draw_current(side, map);
	    }
	    invert_unit_subarea(side, map, map->curx, map->cury);
	    /* (should wait a moment) */
	    invert_unit_subarea(side, map, map->curx, map->cury);
	    draw_row(side, map, map->curx, map->cury, 1, TRUE);
	    if (map->curunit) {
		draw_current(side, map);
	    }
	}
    } else {
	beep(side);
    }
}

/* Create a new map, of standard size and zoom. */
/* (should clone last map in list perhaps?) */

void
do_x_map(side, map)
Side *side;
Map *map;
{
    Map *map2;

    map2 = create_map(side, 5, NULL);
}

/* Close a map window permanently. */

void
do_x_map_close(side, map)
Side *side;
Map *map;
{
    /* (should trash map and popdown) */
    notify(side, "can't close a map yet");
}

void
do_x_reverse_video(side, map)
Side *side;
Map *map;
{
    if (side->ui->monochrome) {
	side->ui->bonw = !side->ui->bonw;
	set_colors(side);
	reset_color_state(side);
	redraw(side);
    } else {
	cmd_error(side, "Reverse video is only for monochrome!");
    }
}

void
do_x_unit_closeup(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;
    UnitCloseup *unitcloseup;
    REQUIRE_UNIT(side, map);
    
    unitcloseup = find_unit_closeup(unit, side);
    if (!unitcloseup)
      unitcloseup = create_unit_closeup(unit, side, map);
    draw_unit_closeup(unitcloseup, side);
}

void
do_x_orders_popup(side, map)
Side *side;
Map *map;
{
  create_orders_window(side, map);
}

void
do_x_side_closeup(side, map)
Side *side;
Map *map;
{
    Unit *unit = map->curunit;
    SideCloseup *sidecloseup;
    REQUIRE_UNIT(side, map);
    
    sidecloseup = find_side_closeup(unit->side, side);
    if (!sidecloseup)
      sidecloseup = create_side_closeup(unit->side, side, map);
    draw_side_closeup(sidecloseup, side);
}

/* Create a new world map (a regular map zoomed to display the whole
   world at once). */

void
do_x_world_map(side, map)
Side *side;
Map *map;
{
    Map *wmap;

    wmap = create_map(side, -1, NULL);
}

void
do_x_zoom_in(side, map)
Side *side;
Map *map;
{
    zoom_in_out(side, map, ZOOM_IN);
}

void
do_x_zoom_out(side, map)
Side *side;
Map *map;
{
    zoom_in_out(side, map, ZOOM_OUT);
}

#ifdef DESIGNERS

static void aux_design PARAMS ((Side *side, Map *map, int cancel));
static void aux_set_terrain_type PARAMS ((Side *side, Map *map, int cancel));
static void aux_set_unit_type PARAMS ((Side *side, Map *map, int cancel));
static int check_designer_status PARAMS ((Side *side, char *str));

int designed_on = FALSE;

void enable_in_unit_type_list PARAMS ((Side *side, Map *map, int u, int flag));

static void really_do_design PARAMS ((Side *side));

void
do_design(side, map)
Side *side;
Map *map;
{
    int u;
    Map *map2;

    if (!side->designer) {
	if (!designed_on) {
	    ask_bool(side, map, "Do you really want to start designing?",
		     FALSE, aux_design);
	} else {
	    really_do_design(side);
	}
    } else {
	become_nondesigner(side);
	for_all_maps(side, map2) {
	    map2->seeall = (all_see_all || side->may_set_see_all);
	    update_controls(side, map2);
	    for_all_unit_types(u) {
		enable_in_unit_type_list(side, map2, u, FALSE);
	    }
	}
	popdown_design(side);
    }
}

static void
aux_design(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    if (cancel)
      return;
    if (grok_bool(side, map)) {
	really_do_design(side);
    } else {
	/* nothing to do if we said not to design */
    }
}

static void
really_do_design(side)
Side *side;
{
    int u;
    Map *map2;

    become_designer(side);
    for_all_maps(side, map2) {
	map2->seeall = (all_see_all || side->may_set_see_all);
	update_controls(side, map2);
	for_all_unit_types(u) {
	    enable_in_unit_type_list(side, map2, u, TRUE);
	}
    }
    popup_design(side);
}

void
do_gdl(side, map)
Side *side;
Map *map;
{
    if (cmdargstr)
      interp_form(NULL, read_form_from_string(cmdargstr, NULL, NULL));
    /* (should broadcast to all?) */
}

static int
check_designer_status(side, str)
Side *side;
char *str;
{
    if (side->designer) {
	return TRUE;
    } else {
	cmd_error(side, "You're not a designer, can't %s!", str);
	return FALSE;
    }
}

void
do_x_set_terrain_type(side, map)
Side *side;
Map *map;
{
    int numtypes;

    if (!check_designer_status(side, "set terr paint type"))
      return;
    numtypes = ask_terrain_type(side, map, "Type to paint: ", NULL,
				aux_set_terrain_type);
    switch (numtypes) {
      case 0:
	beep(side);
	break;
      case 1:
	side->ui->curttype = map->tvec[0];
	if (map->prefixarg >= 0)
	  side->ui->curbrushradius = map->prefixarg;
	notify(side, "will now be painting %d-radius %s.",
	       side->ui->curbrushradius, t_type_name(side->ui->curttype));
	break;
      default:
	break;
    }
}

static void
aux_set_terrain_type(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int t2;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Not being helpful yet.");
	map->modalhandler = aux_set_terrain_type;
	return;
    }
    if (grok_terrain_type(side, map, &t2)) {
	if (t2 != NONUTYPE) {
	    side->ui->curttype = t2;
	    if (map->prefixarg >= 0)
	      side->ui->curbrushradius = map->prefixarg;
	    notify(side, "will now be painting %d-radius %s.",
		   side->ui->curbrushradius, t_type_name(side->ui->curttype));
	}
    } else {
	/* beep? */
    }
}

/* Command to paint terrain. */

void
do_x_paint_terrain(side, map)
Side *side;
Map *map;
{
    if (!check_designer_status(side, "paint terrain"))
      return;
    paint_cell(side, map->curx, map->cury,
	       side->ui->curbrushradius, side->ui->curttype);
}

/* Command to set the current unit type to create. */

void
do_x_set_unit_type(side, map)
Side *side;
Map *map;
{
    int numtypes;

    if (!check_designer_status(side, "set unit type"))
      return;
    numtypes = ask_unit_type(side, map, "Type to create: ", NULL,
			     aux_set_unit_type);
    switch (numtypes) {
      case 0:
	beep(side);
	break;
      case 1:
	side->ui->curutype = map->uvec[0];
	if (map->prefixarg >= 0)
	  side->ui->cursidenumber = map->prefixarg;
	notify(side, "will now be creating %s %s units",
	       side_adjective(side_n(side->ui->curusidenumber)),
	       u_type_name(side->ui->curutype));
	break;
      default:
	break;
    }
}

static void
aux_set_unit_type(side, map, cancel)
Side *side;
Map *map;
int cancel;
{
    int u2;

    if (cancel)
      return;
    if (map->inpch == '?') {
	notify(side, "Not being helpful yet.");
	map->modalhandler = aux_set_unit_type;
	return;
    }
    if (grok_unit_type(side, map, &u2)) {
	if (u2 != NONUTYPE) {
	    side->ui->curutype = u2;
	    if (map->prefixarg >= 0)
	      side->ui->cursidenumber = map->prefixarg;
	    notify(side, "will now be creating side %d %s units",
		   side->ui->cursidenumber, u_type_name(side->ui->curutype));
	}
    } else {
	/* beep? */
    }
}

/* Command to create and place a unit. */

void
do_x_add_unit(side, map)
Side *side;
Map *map;
{
    Unit *unit;

    if (!check_designer_status(side, "create units"))
      return;
    unit = designer_create_unit(side,
				side->ui->curutype, side->ui->cursidenumber,
				map->curx, map->cury);
    if (unit != NULL) {
	set_current_unit(side, map, unit);
    } else {
	cmd_error(side, "Unit creation failed!");
    }
}

#endif /* DESIGNERS */

#ifdef DEBUGGING

/* Debugging-related commands. */

/* General debugging toggles. */

void
do_debug(side, map)
Side *side;
Map *map;
{
#ifndef Debug
    Debug = !Debug;
#endif
}

void
do_debugg(side, map)
Side *side;
Map *map;
{
#ifndef DebugG
    DebugG = !DebugG;
#endif
    XSynchronize(side->ui->dpy, DebugG);
}

void
do_debugm(side, map)
Side *side;
Map *map;
{
#ifndef DebugM
    DebugM = !DebugM;
#endif
}

/* Pretend to be a monochrome screen - use for comparing color
   and b/w appearance of screens. */

void
do_x_fake_mono(side, map)
Side *side;
Map *map;
{
    side->ui->monochrome = !side->ui->monochrome;
    side->ui->bonw = BLACKONWHITE;
    set_colors(side);
    reset_color_state(side);
    redraw(side);
}

#endif /* DEBUGGING */

/* Generic command error feedback. */

static void
#ifdef ANSI_PROTOTYPES
cmd_error(Side *side, char *fmt, ...)
#else
cmd_error(side, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
Side *side;
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
#ifdef ANSI_PROTOTYPES
    va_list ap;

    va_start(ap, fmt);
    vnotify(side, fmt, ap);
    va_end(ap);
#else
    notify(side, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    /* Only beep once, even if a command generates multiple error messages. */
    if (side->ui->beepcount++ < 1)
      beep(side);
}
