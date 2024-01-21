/* Commands for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void update_debugging PARAMS ((void));
extern void toggle_debugging PARAMS ((int *flagp));
extern int can_build_or_help PARAMS ((Unit *unit));
extern int can_produce PARAMS ((Unit *unit));
extern Task *create_produce_task PARAMS ((int m, int n));
extern void push_produce_task PARAMS ((Unit *unit, int m, int n));
/* Only defined if ansi */
extern void vnotify(Side *side, char *fmt, va_list ap);
#include "print.h"
#include "macconq.h"
extern void enable_command(void);
extern void set_construction_run_length(int len);

/* Define in order to get profiling controls. */
#undef PROFILING

#ifdef THINK_C
#include <profile.h>
#endif

#ifdef PROFILING
extern int _trace;
#endif

extern WindowPtr playersetupwin;

extern int forcedtoresign;

extern int modal_construction;

extern WindowPtr window_behind_construction;

int position_set_modally;

Point modal_point;

Map *modal_map;

#ifdef DESIGNERS
#define side_may_select(unit) (in_play(unit) && ((unit)->side == dside || dside->designer))
#define valid_selection(unit) (in_play(unit) && ((unit)->side == dside || dside->designer))
#else
#define side_may_select(unit) (in_play(unit) && ((unit)->side == dside))
#define valid_selection(unit) (in_play(unit) && ((unit)->side == dside))
#endif

extern char *cursavename;

#undef DEF_CMD
#define DEF_CMD(letter,name,args,FN,help) extern void FN(void);

#include "cmd.def"

#include "maccmd.def"

/* Local function declarations. */

static int do_one_add_terrain(Unit *unit);
static int do_one_ai_control(Unit *unit);
static void unit_do_build_2(Unit *unit);
extern int do_one_clear_plan(Unit *unit);
static int do_one_delay(Unit *unit);
extern int do_one_detach(Unit *unit);
extern int do_one_detonate(Unit *unit);
extern int do_one_dir_move(Unit *unit);
extern int do_one_dir_multiple_move(Unit *unit);
extern int do_one_disband(Unit *unit);
extern int do_one_disembark(Unit *unit);
extern int do_one_embark(Unit *unit);
extern int do_one_give(Unit *unit);
extern int do_one_give_unit(Unit *unit);
static int do_one_occupant(Unit *unit);
static int do_one_remove_terrain(Unit *unit);
static int do_one_reserve(Unit *unit);
extern int do_one_return(Unit *unit);
extern int do_one_set_name(Unit *unit);
static int do_one_asleep(Unit *unit);
extern int do_one_take(Unit *unit);

static void resign_the_game(int forced);

static void cmd_error(Side *side, char *fmt, ...);

/* Static (local) variables. */

static int tmpcmdarg;
static int tmprecurse;

static int tmpcmdx, tmpcmdy, tmpcmddir;

/* This is the actual key typed, for use if several keyboard commands
   share a single function. */

static char tmpkey;

/* Prefixed numeric argument to commands. */

static int prefixarg;

static int tmpdir;

static char *cmdargstr;

/* This flag is set to prevent running an "other" command when already
   executing an "other" command. */

static int doingother = FALSE;

typedef struct cmdtab {
    char fchar;                 /* character to match against */
    char *name;                 /* full name of command */
    void (*fn)(void);           /* pointer to command's function */
    char *help;                 /* short documentation string */
} CmdTab;

#define C(c) ((c)-0x40)

#undef DEF_CMD
#define DEF_CMD(LETTER,NAME,args,FN,HELP) { LETTER, NAME, FN, HELP },

/* The generic command table. */

CmdTab commands[] = {

#include "maccmd.def"

#include "cmd.def"

  { 0, NULL, NULL, NULL }
};

/* The Mac-specific command table. */

CmdTab mac_commands[] = {

#include "maccmd.def"

  { 0, NULL, NULL, NULL }
};

/* Given a character, find a command for it and execute. */

void
do_keyboard_command(int key)
{
	CmdTab *cmd;
    void (*fn)(void);

	DGprintf("Typed '%c' (0x%x)\n", key, key);
	if (between('0', key, '9')) {
		/* Add a decimal digit to the prefix argument. */
		if (prefixarg < 0)
		  prefixarg = 0;
		prefixarg += prefixarg * 10 + (key - '0');
		/* (should add some sort of feedback) */
	} else {
		/* Look through the generic command table. */
	    for (cmd = commands; cmd->name != NULL; ++cmd) {
			if (key == cmd->fchar) {
		    	if ((fn = cmd->fn) == NULL) {
					run_warning("no command function for %s (0x%x)?", cmd->name, key);
					return;
		    	}
		    	tmpkey = key;
				(*fn)();
				/* Reset the prefix argument. */
				prefixarg = -1;
				return;
			}
		}
	}
}

void
execute_named_command(char *cmdstr)
{
	char *cmdname;
	CmdTab *cmd;
    void (*fn)(void);

    parse_long_name_command(cmdstr, &cmdname, &cmdargstr, copy_string(cmdstr)); 
    if (empty_string(cmdname)) {
		notify(dside, "No command name.");
		beep();
		return;
	}
    for (cmd = commands; cmd->name != NULL; ++cmd) {
		if (strcmp(cmdname, cmd->name) == 0) {
			fn = cmd->fn;
	    	if (fn == NULL) {
				run_warning("no command function for %s?", cmd->name);
				return;
	    	}
	    	/* Use the command's char as the apparent key. */
	    	tmpkey = cmd->fchar;
			(*fn)();
			/* Reset the prefix argument. */
			prefixarg = -1;
			return;
		}
	}
	notify(dside, "Command \"%s\" not recognized.", cmdname);
	beep();
}

void
describe_commands(int arg, char *key, char *buf)
{
    describe_command_table(arg, key, buf, commands);
    /* (should split out Mac-specific commands??) */
}

void
describe_command_table(int arg, char *key, char *buf, CmdTab *cmdtab)
{
	CmdTab *cmd;

	strcat(buf, "Single-key commands:\n\n");
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
		describe_command(cmd->fchar, cmd->name, cmd->help, TRUE, buf);
    }
    strcat(buf, "\nLong name commands:\n\n");
    for (cmd = cmdtab; cmd->name != NULL; ++cmd) {
		describe_command (cmd->fchar, cmd->name, cmd->help, FALSE, buf);
    }
}

int
get_a_position(Map **mapp, int *xp, int *yp, int *dirp)
{
	Point at;

	if (position_set_modally) {
		position_set_modally = FALSE;
		*mapp = modal_map;
		return m_nearest_boundary(*mapp, modal_point.h, modal_point.v, xp, yp, dirp);
	} else if ((*mapp = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&at);
		return m_nearest_boundary(*mapp, at.h, at.v, xp, yp, dirp);
	}
	return FALSE;
}

int
get_a_unit(Map **mapp, Unit **unitp)
{
	Point at;

	if (position_set_modally) {
		position_set_modally = FALSE;
		*mapp = modal_map;
		return m_nearest_unit(*mapp, modal_point.h, modal_point.v, unitp);
	} else if ((*mapp = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&at);
		return m_nearest_unit(*mapp, at.h, at.v, unitp);
	}
	return FALSE;
}

/* Start of alphabetized commands. */

void
do_add_player()
{
    request_additional_side(cmdargstr);
}

void
do_add_terrain()
{
    int x, y, dir;
	Map *map;

	if (get_a_position(&map, &x, &y, &dir)) {
		tmpcmdx = x;  tmpcmdy = y;  tmpcmddir = dir;
		apply_to_all_selected(do_one_add_terrain, TRUE);
		return;
	}
	beep();
}

static int
do_one_add_terrain(Unit *unit)
{
	int u, t, t1, x, y, dir, tfirst = -1, numtypes, possibles[MAXTTYPES];

    u = unit->type;
    numtypes = 0;
	for_all_terrain_types(t) {
		if (ut_acp_to_add_terrain(u, t) > 0) {
			possibles[numtypes++] = t;
		}
	}
	if (numtypes == 0) {
		return FALSE;
	} else if (numtypes == 1) {
		t1 = possibles[0];
	} else {
		/* should have better way to choose type */
		if (between(0, prefixarg, numtypes - 1))
		  t1 = possibles[prefixarg];
		else {
			notify(dside, "Prefix should be 0 to %d", numtypes - 1); 
			return FALSE;
		}
	}
	/* (should put following in ui.c?) */
	switch (t_subtype(t1)) {
		case cellsubtype:
			if (valid(check_alter_cell_action(unit, unit, tmpcmdx, tmpcmdy, t1))) {
				if (prep_alter_cell_action(unit, unit, tmpcmdx, tmpcmdy, t1))
				  return TRUE;
			}
			break;
		case bordersubtype:
		case connectionsubtype:
		    if (distance(tmpcmdx, tmpcmdy, unit->x, unit->y) <= ut_alter_range(u, t1)) {
				x = tmpcmdx;  y = tmpcmdy;
				dir = tmpcmddir;
			} else {
				x = unit->x;  y = unit->y;
				dir = approx_dir(tmpcmdx - unit->x, tmpcmdy - unit->y);
			}
			if (valid(check_add_terrain_action(unit, unit, x, y, dir, t1))) {
				if (prep_add_terrain_action(unit, unit, x, y, dir, t1))
				  return TRUE;
			}
			break;
		case coatingsubtype:
			break;
	}
	return FALSE;
}

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
	do_attack_command();
}

void
do_auto()
{
	apply_to_all_selected(do_one_ai_control, TRUE);	
}

static int
do_one_ai_control(Unit *unit)
{
    int newval;

	if (unit->plan) {
		if (prefixarg < 0)
		  newval = !unit->plan->aicontrol;
		else if (prefixarg == 0)
		  newval = 0;
		else
		  newval = 1;
		set_unit_ai_control(dside, unit, newval, FALSE);
		/* a hack */
		unit->plan->waitingfortasks = !unit->plan->aicontrol;
	}
	return TRUE;
}

void
do_build()
{
	int i;
	Map *map;  List *list;  UnitCloseup *closeup;
	Unit *unit;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		for (i = 0; i < map->numselections; ++i) {
			unit = map->selections[i];
			if (unit != NULL && can_build_or_help(unit)) {
				unit_do_build_2(unit);
				return;
			}
		}
		return;
	}
	list = list_from_window(FrontWindow());
	if (list != NULL) {
		unit = selected_unit_in_list(list);
		if (unit != NULL && can_build_or_help(unit)) {
			unit_do_build_2(unit);
			return;
		}
		return;
	}
	closeup = unit_closeup_from_window(FrontWindow());
	if (closeup != NULL) {
		unit = closeup->unit;
		if (unit != NULL && can_build_or_help(unit)) {
			unit_do_build_2(unit);
			return;
		}
		return;
	}
	/* No way to figure out the unit to be building with, so complain. */
	beep();
}

void
unit_do_build_2(Unit *unit)
{
    extern int editedrunlength;

	modal_construction = TRUE;
	window_behind_construction = FrontWindow();
	if (prefixarg > 0)
	  set_construction_run_length(prefixarg);
	if (prefixarg > 0)
	  editedrunlength = prefixarg;
	else
	  editedrunlength = -1;
	enable_construction();
	select_unit_in_construction_window(unit);
	select_type_in_construction_window(favored_type(unit));
}

/* Create and/or bring up the construction planning window. */

void
enable_construction()
{
	window_behind_construction = FrontWindow();
	if (constructionwin == nil) {
		create_construction_window();
	}
	if (constructionwin != nil) {
		reinit_construction_lists();
		ShowWindow(constructionwin);
		SelectWindow(constructionwin);
	}
}

int
do_one_clear_plan(Unit *unit)
{
	if (unit->plan) {
		set_unit_plan_type(dside, unit, PLAN_NONE);
		return TRUE;
	}
	return FALSE;
}

void
do_clear_plan()
{
	apply_to_all_selected(do_one_clear_plan, TRUE);	
}

void
do_copying()
{
	help_dialog(copying_help_node);
}

void
do_delay()
{
	apply_to_all_selected(do_one_delay, TRUE);
}

int
do_one_delay(Unit *unit)
{
	if (unit->plan) {
	    delay_unit(unit, TRUE);
		return TRUE;
	}
	return FALSE;
}

void
do_detach()
{
	apply_to_all_selected(do_one_detach, TRUE);
}

int
do_one_detach(Unit *unit)
{
	if (!completed(unit)) {
		return FALSE;
	} else if (valid(check_transfer_part_action(unit, unit, unit->hp / 2, NULL))) {
		prep_transfer_part_action(unit, unit, unit->hp / 2, NULL);
		return TRUE;
	} else {
		/* try to find a nearby unit to do it */
	}
	return FALSE;
}

void
do_detonate()
{
	do_detonate_command();
}

/* Command all selected mobile units to move in a given direction. */

/* The function that gets called on each selected unit. */

int
do_one_dir_move(Unit *unit)
{
    int nx, ny;

	if (mobile(unit->type)) {
		if (point_in_dir(unit->x, unit->y, tmpdir, &nx, &ny)) {
			return advance_into_cell(dside, unit, nx, ny, unit_at(nx, ny));
		}
		return FALSE;
	}
	return FALSE;
}

/* The command function proper. */

void
do_dir()
{
	int ndirs, dir1, dir2, modif;
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		ndirs = char_to_dir(tmpkey, &dir1, &dir2, &modif);
		if (ndirs >= 1) {
			tmpdir = dir1;
		} else {
			beep();
			return;
		}
		apply_to_all_selected(do_one_dir_move, TRUE);
	}
}

/* Command all selected mobile units to move in a given direction. */

/* The function that gets called on each selected unit. */

int
do_one_dir_multiple_move(Unit *unit)
{
	if (mobile(unit->type)) {
		set_move_dir_task(unit, tmpdir, (prefixarg <= 0 ? 9999 : prefixarg));
		return TRUE;
	}
	return FALSE;
}

/* The command function proper. */

void
do_dir_multiple()
{
	int ndirs, dir1, dir2, modif;
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		ndirs = char_to_dir(tmpkey, &dir1, &dir2, &modif);
		if (ndirs >= 1) {
			tmpdir = dir1;
		} else {
			beep();
			return;
		}
		apply_to_all_selected(do_one_dir_multiple_move, TRUE);
	}
}

void
do_disband()
{
	apply_to_all_selected(do_one_disband, TRUE);
}

int
do_one_disband(Unit *unit)
{
    return disband_unit(dside, unit);
}

void
do_disembark()
{
	apply_to_all_selected(do_one_disembark, TRUE);
}

int
do_one_disembark(Unit *unit)
{
	Unit *transport = unit->transport;

	if (transport == NULL)
	  return FALSE;
	/* Try moving into the transport's transport, if there is one. */
	if (transport->transport != NULL
		&& can_occupy(unit, transport->transport)) {
		prep_enter_action(unit, unit, transport->transport);
		/* (should be able to set up task if can't do action immediately) */
		return TRUE;
	}
	/* Try moving into the open in the cell. */
	if (!inside_area(unit->x, unit->y))
	  return FALSE;
	if (can_occupy_cell(unit, unit->x, unit->y)
		|| can_occupy_conn(unit, unit->x, unit->y, unit->z)) {
		prep_move_action(unit, unit, unit->x, unit->y, unit->z);
		/* (should be able to set up task if can't do action immediately) */
		return TRUE;
	}
	return FALSE;
}

void
do_distance()
{
    notify(dside, "Click, then drag to location to which you want the distance.");
	query_position_modally(DISTANCE_MODAL);
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
	beep();
}

void
do_draw_willingness()
{
    if (prefixarg < 0)
	  prefixarg = 1;
    set_willing_to_draw(dside, (prefixarg ? 1 : 0));
}

void
do_embark()
{
	apply_to_all_selected(do_one_embark, TRUE);
}

int
do_one_embark(Unit *unit)
{
	int x = unit->x, y = unit->y;
	Unit *transport = unit->transport, *unit2, *unit3;

	/* Note that occupation tests check for unit == transport case,
	   so not necessary in the code below. */
	if (transport == NULL) {
		/* Unit is in the open. */
		for_all_stack(x, y, unit2) {
			if (unit2 != transport && can_occupy(unit, unit2)) {
				prep_enter_action(unit, unit, unit2);
				return TRUE;
			}
		}
		for_all_stack(x, y, unit2) {
			for_all_occupants(unit2, unit3) {
				if (unit3 != transport && can_occupy(unit, unit3)) {
					prep_enter_action(unit, unit, unit3);
					return TRUE;
				}
			}
		}
	} else {
		for_all_occupants(transport, unit2) {
			if (unit2 != transport && can_occupy(unit, unit2)) {
				prep_enter_action(unit, unit, unit2);
				return TRUE;
			}
		}
		if (transport->transport == NULL) {
			for_all_stack(x, y, unit2) {
				if (unit2 != transport && can_occupy(unit, unit2)) {
					prep_enter_action(unit, unit, unit2);
					return TRUE;
				}
			}
			for_all_stack(x, y, unit2) {
				for_all_occupants(unit2, unit3) {
					if (unit3 != transport && can_occupy(unit, unit3)) {
						prep_enter_action(unit, unit, unit3);
						return TRUE;
					}
				}
			}
		} else {
			for_all_occupants(transport->transport, unit2) {
				if (unit2 != transport && can_occupy(unit, unit2)) {
					prep_enter_action(unit, unit, unit2);
					return TRUE;
				}
			}
		}
	}
	return FALSE;
}

void
do_end_turn()
{
	/* <return> is also interpreted by dialogs, so special-case this, depending
	   on which window was in front. */
	if (FrontWindow() == constructionwin) {
		Point pt;
		extern ControlHandle constructbutton;
		
		pt.h = (*constructbutton)->contrlRect.left + 8;
		pt.v = (*constructbutton)->contrlRect.top + 8;
		do_mouse_down_construction(pt, 0);
	} else {
		finish_turn(dside);
	}
}

void
do_fire()
{
	do_fire_command();
}

void
do_fire_into()
{
	do_fire_into_command();
}

void
do_follow_action()
{
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		map->follow_action = !map->follow_action;
	}
}

void
do_force_global_replan()
{
	force_global_replan(dside);
}

void
do_give()
{
    /* (should use argument) */
	apply_to_all_selected(do_one_give, TRUE);
}

void
do_give_unit()
{
	int sn = 0;
	Side *side;

	side = side_n(sn);
	apply_to_all_selected(do_one_give_unit, TRUE);
}

void
do_help()
{
    /* Bring up the help window at wherever it was, if already
       created, otherwise will start at topics node. */
	help_dialog(NULL);
}

void
do_message()
{
	message_dialog();
}

/* Dialog for the input of a textual message. */

/* (should add way to specify which sides to receive this) */

void
message_dialog()
{
	short done = FALSE, ditem;
	char *msg = NULL;
	DialogPtr win;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	win = GetNewDialog(dMessage, NULL, (DialogPtr) -1L);
	ShowWindow(win);
	while (!done) {
		draw_default_button(win, diMessageOK);
		SetCursor(&QD(arrow));
		ModalDialog(NULL, &ditem);
		switch (ditem) {
			case diMessageOK:
				GetDItem(win, diMessageText, &itemtype, &itemhandle, &itemrect);
				msg = get_string_from_item(itemhandle);
				/* Fall into next case. */
			case diMessageCancel:
				done = TRUE;
				break;
		}
	}
	/* Close down the dialog (*before* executing any command). */
	DisposDialog(win);
	/* Now send the message (if it wasn't cancelled) */
	if (msg != NULL) {
		send_message(dside, ALLSIDES, msg);
	}
}

void
do_move_to()
{
    notify(dside, "Click on location to which you want to move.");
	query_position_modally(MOVE_TO_MODAL);
}

extern int do_one_move_to(Unit *unit);

int
do_move_to_command()
{
	int x, y, dir;
	Map *map;

	if (get_a_position(&map, &x, &y, &dir)) {
		tmpcmdx = x;  tmpcmdy = y;
		apply_to_all_selected(do_one_move_to, TRUE);
		return TRUE;
	}
	return FALSE;
}

int
do_one_move_to(Unit *unit)
{
#ifdef DESIGNERS
	if (dside->designer) {
		designer_teleport(unit, tmpcmdx, tmpcmdy, NULL);
		return TRUE;
	}
#endif /* DESIGNERS */
	set_move_to_task(unit, tmpcmdx, tmpcmdy);
	return TRUE;
}

int
do_one_set_name(Unit *unit)
{
	return unit_rename_dialog(unit);
}

void
do_name()
{
	apply_to_all_selected(do_one_set_name, TRUE);
}

int
do_one_occupant(Unit *unit)
{
	Unit *nextocc;
	Map *map;

	nextocc = find_next_occupant(unit);
	if (nextocc != unit) {
		map = map_from_window(FrontWindow());
		if (map != NULL) {
			unselect_unit_on_map(map, unit);
			select_unit_on_map(map, nextocc);
			/* Overkill, but draw_row alone not as desirable? */
			update_cell_display(dside, unit->x, unit->y, TRUE);
		}
	}
	return TRUE;
}

void
do_occupant()
{
	apply_to_all_selected(do_one_occupant, TRUE);
}

void
do_other()
{
	/* Don't allow recursion with this command. */
	if (!doingother) {
		doingother = TRUE;
		enable_command();
		doingother = FALSE;
	} else {
		beep();
	}
}

/* Dialog for the input of a textual command. */

WindowPtr commandwin;

ControlHandle dobutton;

TEHandle command_text = nil;

Rect commandtextrect;

void create_command_dialog(void);

static void get_command_and_do(void);

void
enable_command()
{
	if (commandwin == nil) {
		create_command_dialog();
	}
	if (commandwin != nil) {
		ShowWindow(commandwin);
		SelectWindow(commandwin);
	}
}

void
create_command_dialog()
{
	Rect tmprect;

	if (hasColorQD) {
		commandwin = GetNewCWindow(wCommand, NULL, (WindowPtr) -1L);
	} else {
		commandwin = GetNewWindow(wCommand, NULL, (WindowPtr) -1L);
	}
	dobutton = GetNewControl(cCommandDoButton, commandwin);
	SetPort(commandwin);
	tmprect = commandwin->portRect;
	commandtextrect = tmprect;
	commandtextrect.bottom -= 30;
	InsetRect(&commandtextrect, 5, 5);
	/* Create and clear the TextEdit record. */
	command_text = TENew(&commandtextrect, &commandtextrect);
	TESetSelect(0, 32767, command_text);
	TEDelete(command_text);
	ShowWindow(commandwin);
}

void
draw_command()
{
	Rect tmprect;

	TEUpdate(&(commandwin->portRect), command_text);
	tmprect = commandtextrect;
	InsetRect(&tmprect, -1, -1);
	FrameRect(&tmprect);
}

void
activate_command(int activate)
{
	if (activate)
	  TEActivate(command_text);
	else
	  TEDeactivate(command_text);
}

int
do_key_down_command(key)
int key;
{
	if (key == 13 || key == 3) {
		get_command_and_do();
	} else {
		TEKey(key, command_text);
	}
	return TRUE;
}

void
do_mouse_down_command(Point mouse, int mods)
{
	short part;
	ControlHandle control;

	part = FindControl(mouse, commandwin, &control);
	if (control == dobutton) {
		get_command_and_do();
	} else if (PtInRect(mouse, &commandtextrect)) {
		TEClick(mouse, mods, command_text);
	}
}

static void
get_command_and_do()
{
	int len;
	char buffer[BUFSIZE];
	CharsHandle text;

	text = TEGetText(command_text);
	len = min((*command_text)->teLength, BUFSIZE);
	strncpy(buffer, *text, len);
	buffer[len] = '\0';
	if (!empty_string(buffer)) {
		execute_named_command(buffer);
	} else {
		notify(dside, "No command.");
		beep();
	}
}

void
do_print_view()
{
	dump_ps_view(dside, NULL, "View PS");
}

static void unit_do_produce_2(Unit *unit);

void
do_produce()
{
	int i;
	Map *map;  List *list;  UnitCloseup *closeup;
	Unit *unit;

	/* Try different alternatives to find a unit. */
	map = map_from_window(FrontWindow());
	if (map != NULL) {
		for (i = 0; i < map->numselections; ++i) {
			unit = map->selections[i];
			if (unit != NULL && can_produce(unit)) {
				unit_do_produce_2(unit);
				return;
			}
		}
		return;
	}
	list = list_from_window(FrontWindow());
	if (list != NULL) {
		unit = selected_unit_in_list(list);
		if (unit != NULL && can_produce(unit)) {
			unit_do_produce_2(unit);
			return;
		}
		return;
	}
	closeup = unit_closeup_from_window(FrontWindow());
	if (closeup != NULL) {
		unit = closeup->unit;
		if (unit != NULL && can_produce(unit)) {
			unit_do_produce_2(unit);
			return;
		}
		return;
	}
	/* No way to figure out the unit to be producing with, so complain. */
	beep();
}

static void
unit_do_produce_2(Unit *unit)
{
	int m, n;

    n = 9999;
    if (prefixarg > 0)
	  n = prefixarg;
	for_all_material_types(m) {
		if (um_acp_to_produce(unit->type, m) > 0) {
			push_produce_task(unit, m, n);
			return;
		}
	}
}

void
do_quit()
{
	quit_the_game();
}

static int allsumx, allsumy, unitcount;

static int
add_unit_position(Unit *unit)
{
	allsumx += unit->x;  allsumy += unit->y;
	++unitcount;
	return TRUE;
}

void
do_recenter()
{
	int avgx, avgy;
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		allsumx = allsumy = 0;
		unitcount = 0;
		apply_to_all_selected(add_unit_position, FALSE);
		if (unitcount == 0) {
			beep();
			return;
		}
		avgx = allsumx / unitcount;  avgy = allsumy / unitcount;
		set_focus(map, avgx, avgy);
	}
}

void
set_focus(Map *map, int x, int y)
{
	if (!inside_area(x, y))
	  return;
	set_view_focus(map->vp, x, y);
	m_center_on_focus(map);
	set_map_scrollbars(map);
	force_map_update(map);
}

/* Recalculate and redraw everything. */

void
do_refresh()
{
	Map *map;
	List *list;
	UnitCloseup *closeup;

	reset_coverage();
	reset_all_views();
	compute_all_feature_centroids();
	/* Force updates to all open windows. */
	force_update(gamewin);
	force_update(noticewin);
	force_update(historywin);
	force_update(constructionwin);
	force_update(helpwin);
	for_all_maps(map) {
		force_update(map->window);
	}
	for_all_lists(list) {
		force_update(list->window);
	}
	for_all_unit_closeups(closeup) {
		force_update(closeup->window);
	}
}

void
do_remove_terrain()
{
    int x, y, dir;
	Map *map;
	
	if (get_a_position(&map, &x, &y, &dir)) {
		tmpcmdx = x;  tmpcmdy = y;  tmpcmddir = dir;
		apply_to_all_selected(do_one_remove_terrain, TRUE);
		return;
	}
	beep();
}

static int
do_one_remove_terrain(Unit *unit)
{
	int u, t, x, y, dir;

	u = unit->type;
	for_all_terrain_types(t) {
		if (ut_acp_to_remove_terrain(u, t) > 0
			&& unit->act
		    && unit->act->acp >= ut_acp_to_remove_terrain(u, t)) {
		    if (distance(tmpcmdx, tmpcmdy, unit->x, unit->y) <= ut_alter_range(u, t)) {
				x = tmpcmdx;  y = tmpcmdy;
				dir = tmpcmddir;
			} else {
				x = unit->x;  y = unit->y;
				dir = approx_dir(tmpcmdx - unit->x, tmpcmdy - unit->y);
			}
			if (valid(check_remove_terrain_action(unit, unit, x, y, dir, t))) {
				if (prep_remove_terrain_action(unit, unit, x, y, dir, t))
				  return TRUE;
			}
		}
	}
	return FALSE;
}

static int
do_one_reserve(Unit *unit)
{
	set_unit_reserve(dside, unit, tmpcmdarg, tmprecurse);
	return TRUE;
}

void
do_reserve_command(int value, int radius, int recurse)
{
	tmpcmdarg = value;
	tmprecurse = recurse;
	apply_to_all_selected(do_one_reserve, TRUE);	
}

void
do_reserve()
{
	do_reserve_command(TRUE, prefixarg, FALSE);
}

static int
do_one_return(Unit *unit)
{
	if (1 /* has a place to return to */) {
		set_resupply_task(unit, NONMTYPE);
		return TRUE;
	}
	return FALSE;
}

void
do_return()
{
	apply_to_all_selected(do_one_return, TRUE);	
}

void
do_resign()
{
	if (endofgame) {
		notify(dside, "Game is already over.");
		beep();
	} else if (!dside->ingame) {
		notify(dside, "You are already out of the game.");
		beep();
	} else if (CautionAlert(aConfirmResign, nil) == aiConfirmResignResign) {
		resign_game(dside, NULL);
	}
}

void
do_save()
{
	save_the_game(FALSE, FALSE);
}

void
do_set_formation()
{
	int i, numcould = 0, numnot = 0;
	Map *map;
	Unit *follower, *leader;
	
	if (get_a_unit(&map, &leader)) {
		if (leader != NULL) {
			for (i = 0; i < map->numselections; ++i) {
				if ((follower = map->selections[i]) != NULL && valid_selection(follower)) {
					if (leader != follower
						&& leader->side == follower->side
						) {
						set_formation(follower, leader, follower->x - leader->x, follower->y - leader->y, 1, 1);
						++numcould;
					} else {
						++numnot;
					}
				}
			}
		} else {
			/* Can't make a formation without a leader. */
			cmd_error(dside, "no leader found");
		}
	}
	/* If nobody could do the command, beep once. */
	if (numcould == 0 && numnot > 0) {
		beep();
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

static int
do_one_asleep(Unit *unit)
{
	set_unit_asleep(dside, unit, tmpcmdarg, tmprecurse);
	return TRUE;
}

void
do_sleep_command(int value, int radius, int recurse)
{
	tmpcmdarg = value;
	tmprecurse = recurse;
	apply_to_all_selected(do_one_asleep, TRUE);	
}

void
do_sleep()
{
	do_sleep_command(TRUE, prefixarg, FALSE);
}

void
do_standing_orders()
{
	int rslt;

	if (cmdargstr) {
		rslt = parse_standing_order(dside, cmdargstr);
		if (rslt < 0)
		  beep();
	} else
	  beep();
}

void
do_surrender_to()
{
	beep();
}

void 
do_survey()
{
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		toggle_survey(map);
	}
}

void
do_take()
{
    /* (should use argument) */
	apply_to_all_selected(do_one_take, TRUE);
}

void
do_take_unit()
{
	beep();
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
	beep();
}

void
do_version()
{
	do_about_box();
}

void
do_wake()
{
	do_sleep_command(FALSE, prefixarg, FALSE);
	do_reserve_command(FALSE, prefixarg, FALSE);
}

void
do_wake_all()
{
	do_sleep_command(FALSE, prefixarg, TRUE);
	do_reserve_command(FALSE, prefixarg, TRUE);
}

void
do_warranty()
{
	help_dialog(warranty_help_node);
}

#ifdef DESIGNERS

/* Toggle the designer mode. */

void
do_design()
{
	if (!dside->designer) {
		enable_designing(FALSE);
	} else {
		disable_designing();
	}
}

void
do_gdl()
{
	if (cmdargstr)
	  interp_form(NULL, read_form_from_string(cmdargstr, NULL, NULL));
	else
	  beep();
	end_printing_forms();
}

#endif

#ifdef DEBUGGING

void
do_debug()
{
	toggle_debugging(&Debug);
	draw_game();
}

void
do_debugg()
{
	toggle_debugging(&DebugG);
	draw_game();
}

void
do_debugm()
{
	toggle_debugging(&DebugM);
	draw_game();
}

void
do_profile()
{
	toggle_profiling();
}

void
do_trace()
{
	toggle_profiling();
#ifdef PROFILING
	_trace = 1;
#endif
}

#endif

/* End of alphabetized commands. */

/* Mac-specific command functions. */

void
do_mac_escape()
{
	map_modal = NO_MODAL;
}

void
do_mac_set_map_angle()
{
	int angle;
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		angle = map->vp->angle;
		if (angle == 90)
		  angle = 30;
		else if (angle == 30)
		  angle = 15;
		else
		  angle = 90;
		map->vp->vertscale = (prefixarg < 1 ? 1 : prefixarg);
		set_view_angle(map->vp, angle);
		force_map_update(map);
	}
}

void
do_mac_zoom_in()
{
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		magnify_map(map, 1);
	} else {
		beep();
	}
}


void
do_mac_zoom_out()
{
	Map *map;

	map = map_from_window(FrontWindow());
	if (map != NULL) {
		magnify_map(map, -1);
	} else {
		beep();
	}
}

#ifdef DEBUGGING

void
toggle_profiling()
{
#ifdef PROFILING
	extern int _profile;
#endif

	Profile = !Profile;

#ifdef PROFILING
#ifdef THINK_C
	if (Profile && !_profile) {
		InitProfile(1000, 100);
		freopen("Xconq.ProfileOut", "w", stdout);
	}
	if (!Profile && _profile) {
		DumpProfile();
		fflush(stdout);
	}
#endif
#endif
}

#endif /* DEBUGGING */

/* Generic command error feedback. */

static void
cmd_error(Side *side, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
    vnotify(side, fmt, ap);
	va_end(ap);
    /* (should) Only beep once, even if a command generates multiple error messages. */
    beep();
}
