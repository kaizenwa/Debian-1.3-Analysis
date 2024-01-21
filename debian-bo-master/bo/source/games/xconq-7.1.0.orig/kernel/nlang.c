/* Interface-independent natural language handling for Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This file should be entirely replaced for non-English Xconq. */
/* (One way to do this would be to call this file "nlang-en.c", then
   symlink this or nlang-fr.c, etc to nlang.c when configuring;
   similarly for help.c.) */
/* (A better way to would be to run all these through a dispatch
   vector, then each player could get messages and such in a
   different language.) */

#include "conq.h"
extern void location_desc PARAMS ((char *buf, Side *side, Unit *unit, int u, int x, int y));
extern int supply_desc PARAMS ((char *buf, Unit *unit, int mrow));

static void init_calendar PARAMS ((void));
static void maybe_mention_date PARAMS ((Side *side));

static int gain_count PARAMS ((Side *side, int u, int r));
static int loss_count PARAMS ((Side *side, int u, int r));
static int atkstats PARAMS ((Side *side, int a, int d));
static int hitstats PARAMS ((Side *side, int a, int d));

static char *tmpnbuf;

static char *tmpdbuf;

static char *pluralbuf;

/* Short names of directions. */

char *dirnames[] = DIRNAMES;

char *unitbuf = NULL;

char *past_unitbuf = NULL;

static char *side_short_title = NULL;

static char *gain_reason_names[] = { "Ini", "Bld", "Cap", "Oth" };

static char *loss_reason_names[] = { "Cbt", "Cap", "Stv", "Acc", "Dis", "Oth" };

/* Calendar handling. */

typedef enum {
    cal_unknown,
    cal_turn,
    cal_usual
} CalendarType;

static CalendarType calendar_type = cal_unknown;

typedef enum {
    ds_second,
    ds_minute,
    ds_hour,
    ds_day,
    ds_week,
    ds_month,
    ds_season,
    ds_year
} UsualDateStepType;

typedef struct a_usualdate {
    int second;
    int minute;
    int hour;
    int day;
    int month;
    int year;
} UsualDate;

static char *usual_date_string PARAMS ((int date));
static void parse_usual_date PARAMS ((char *datestr, UsualDate *udate));

static UsualDateStepType datesteptype;

static int datestep;

static char *months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "???" };

UsualDate *usual_initial;

char *datebuf;

char *turnname;

/* This is the number of types to mention by name; any others will
   just be included in the count of missing images. */

#define NUMTOLIST 5

static char *missinglist = NULL;

/* This array allows for counting up to 4 classes of missing images. */

static int missing[4];

static int totlisted = 0;

/* Initialize things.  Note that this happens before any game is loaded, so
   can't do game-specific init here. */

void
init_nlang()
{
    if (tmpnbuf == NULL)
      tmpnbuf = xmalloc(BUFSIZE);
    if (tmpdbuf == NULL)
      tmpdbuf = xmalloc(BUFSIZE);
    if (pluralbuf == NULL)
      pluralbuf = xmalloc(BUFSIZE);
    if (datebuf == NULL)
      datebuf = xmalloc(BUFSIZE);
}

/* Send a message to everybody who's got a screen. */

#ifdef __STDC__
void
notify_all(char *fmt, ...)
#else
void
notify_all(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    Side *side;

    for_all_sides(side) {
	if (active_display(side)) {
	    maybe_mention_date(side);
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
	    /* Always capitalize first char of notice. */
	    if (islower(tmpnbuf[0]))
	      tmpnbuf[0] = toupper(tmpnbuf[0]);
	    low_notify(side, tmpnbuf);
	}
    }
}

#ifdef __STDC__
void
notify(Side *side, char *fmt, ...)
#else
void
notify(side, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
Side *side;
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    if (!active_display(side))
      return;
    maybe_mention_date(side);
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
    /* Always capitalize first char of notice. */
    if (islower(tmpnbuf[0]))
      tmpnbuf[0] = toupper(tmpnbuf[0]);
    low_notify(side, tmpnbuf);
}

#if defined(__STDC__) || defined(MPW_C)
void
vnotify(side, fmt, ap)
Side *side;
char *fmt;
va_list ap;
{
    if (!active_display(side))
      return;
    maybe_mention_date(side);
    vsprintf(tmpnbuf, fmt, ap);
    /* Always capitalize first char of notice. */
    if (islower(tmpnbuf[0]))
      tmpnbuf[0] = toupper(tmpnbuf[0]);
    low_notify(side, tmpnbuf);
}
#endif /* __STDC__ */

static void
maybe_mention_date(side)
Side *side;
{
    /* Note that last_notice_date defaults to 0, so this means that
       any turn 0 notices will not have a date prepended (which is good). */
    if (g_turn() != side->last_notice_date) {
	sprintf(tmpdbuf, "%s:", absolute_date_string(g_turn()));
	low_notify(side, tmpdbuf);
	side->last_notice_date = g_turn();
    }
}

/* Pad a given buffer with blanks out to the given position, and cut off any
   additional content. */

void
pad_out(buf, n)
char *buf;
int n;
{
    int i, len = strlen(buf);

    if (n < 1)
      return;
    for (i = len; i < n; ++i) {
	buf[i] = ' ';
    }
    buf[n - 1] = '\0';
}

/* Get a char string naming the side.  Doesn't have to be pretty. */

/* (should synth complete name/adjective from other parts of speech) */

char *
side_name(side)
Side *side;
{
    return (side == NULL ? "independent" :
      (side->name ? side->name :
	    (side->adjective ? side->adjective :
	     (side->pluralnoun ? side->pluralnoun :
	      (side->noun ? side->noun :
	       "")))));
}

char *
side_adjective(side)
Side *side;
{
    return (side == NULL ? "independent" :
       (side->adjective ? side->adjective :
	    (side->noun ? side->noun :
	     (side->pluralnoun ? side->pluralnoun :
	      (side->name ? side->name :
	       "")))));
}

char *
short_side_title(side)
Side *side;
{
    if (side_short_title == NULL)
      side_short_title = xmalloc(BUFSIZE);
    if (side == NULL) {
	return " - ";
    } else if (side->name) {
	return side->name;
    } else if (side->pluralnoun) {
	sprintf(side_short_title, "the %s", side->pluralnoun);
    } else if (side->noun) {
	sprintf(side_short_title, "the %s", plural_form(side->noun));
    } else if (side->adjective) {
	sprintf(side_short_title, "the %s side", side->adjective);
    } else {
	return " - ";
    }
    return side_short_title;
}

char *
short_side_title_with_adjective(side, adjective)
Side *side;
char *adjective;
{
    if (side_short_title == NULL)
      side_short_title = xmalloc(BUFSIZE);
    side_short_title[0] = '\0';
    if (side == NULL) {
	return " - ";
    } else if (side->name) {
	if (empty_string(adjective))
	  return side->name;
	else {
	    strcat(side_short_title, adjective);
	    strcat(side_short_title, " ");
	    strcat(side_short_title, side->name);
	}
    } else if (side->pluralnoun) {
	strcat(side_short_title, "the ");
	if (!empty_string(adjective)) {
	    strcat(side_short_title, adjective);
	    strcat(side_short_title, " ");
	}
	strcat(side_short_title, side->pluralnoun);
    } else if (side->noun) {
	strcat(side_short_title, "the ");
	if (!empty_string(adjective)) {
	    strcat(side_short_title, adjective);
	    strcat(side_short_title, " ");
	}
	strcat(side_short_title, side->noun);
    } else if (side->adjective) {
	strcat(side_short_title, "the ");
	if (!empty_string(adjective)) {
	    strcat(side_short_title, adjective);
	    strcat(side_short_title, " ");
	}
	strcat(side_short_title, side->adjective);
	strcat(side_short_title, " side");
    } else {
	return " - ";
    }
    return side_short_title;
}

/* This indicates whether the above routine returns a singular or plural form
   of title. */

int
short_side_title_plural_p(side)
Side *side;
{
    if (side == NULL) {
	return FALSE;
    } else if (side->name) {
	return FALSE;
    } else if (side->pluralnoun) {
	return TRUE;
    } else if (side->noun) {
	return TRUE;
    } else if (side->adjective) {
	sprintf(side_short_title, "The %s side", side->adjective);
	return FALSE;
    } else {
	return FALSE;
    }
}

char *
shortest_side_title(side2, buf)
Side *side2;
char *buf;
{
    if (side2 == NULL) {
	return "-";
    } else if (side2->name) {
	return side2->name;
    } else if (side2->adjective) {
	return side2->adjective;
    } else if (side2->noun) {
	return side2->noun;
    } else {
	sprintf(buf, "(#%d)", side_number(side2));
    }
    return buf;
}

/* (should let NULL observer be assumed objective observer) */

char *
rel_short_side_title(side, side2, n)
Side *side, *side2;
int n;
{
    return "???";
}

char *
rel_long_side_title(side, side2)
Side *side, *side2;
{
    return "???";
}

char *
narrative_side_desc(side, side2)
Side *side, *side2;
{
    return "???";
}

char *
narrative_side_desc_first(side, side2)
Side *side, *side2;
{
    return "???";
}

char *
side_score_desc(buf, side, sk)
char *buf;
Side *side;
Scorekeeper *sk;
{
    if (symbolp(sk->body)
		&& match_keyword(sk->body, K_LAST_SIDE_WINS)) {
	sprintf(buf, "Point Value: %d", side_point_value(side));
    } else {
	/* Compose the generic scorekeeper status display. */
	if (sk->title != NULL) {
	    sprintf(buf, "%s", sk->title);
	} else {
	    sprintf(buf, "SK #%d", sk->id);
	}
	if (sk->scorenum >= 0) {
	    tprintf(buf, ": %d", side->scores[sk->scorenum]);
	}
    }
    return buf;
}

char *
long_player_title(buf, player, thisdisplayname)
char *buf, *thisdisplayname;
Player *player;
{
    buf[0] = '\0';
    if (player == NULL) {
	/* Do nothing */
    } else if (player->displayname != NULL) {
	if (player->name != NULL) {
	    strcat(buf, player->name);
	    strcat(buf, "@");
	}
	if (thisdisplayname != NULL
	    && strcmp(player->displayname, thisdisplayname) == 0) {
	    strcat(buf, "You");
	} else {
	    strcat(buf, player->displayname);
	}
	if (player->aitypename != NULL) {
	    strcat(buf, "(& AI ");
	    strcat(buf, player->aitypename);
	    strcat(buf, ")");
	}
    } else if (player->aitypename != NULL) {
	strcat(buf, "AI ");
	strcat(buf, player->aitypename);
    } else {
	strcat(buf, "-");
    }
    return buf;
}

char *
short_player_title(buf, player, thisdisplayname)
char *buf, *thisdisplayname;
Player *player;
{
    buf[0] = '\0';
    if (player == NULL)
      return buf;
    if (player->name != NULL) {
	strcat(buf, player->name);
    }
    if (player->aitypename != NULL) {
	strcat(buf, ",");
	strcat(buf, player->aitypename);
    }
    if ((player->name != NULL || player->aitypename != NULL)
	&& player->displayname != NULL) {
	strcat(buf, "@");
    }
    if (thisdisplayname != NULL
	&& player->displayname != NULL
	&& strcmp(player->displayname, thisdisplayname) == 0) {
	strcat(buf, "You");
    } else if (player->displayname != NULL) {
	strcat(buf, player->displayname);
    }
    if (strlen(buf) == 0) {
	strcat(buf, "-");
    }
    return buf;
}

void
side_and_type_name(buf, side, u, side2)
char *buf;
Side *side, *side2;
int u;
{
    /* Decide how to identify the side. */
    if (side2 == NULL) {
	sprintf(buf, "independent ");
    } else if (side == side2) {
	sprintf(buf, "your ");
    } else {
	/* (this could be more elaborate) */
	sprintf(buf, "%s ",
		(side2->adjective ? side2->adjective :
		 (side2->noun ? side2->noun : "?")));
    }
    /* Glue the pieces together and return it. */
    strcat(buf, u_type_name(u));
}

/* Build a short phrase describing a given unit to a given side,
   basically consisting of indication of unit's side, then of unit itself. */

char *
unit_handle(side, unit)
Side *side;
Unit *unit;
{
    char *utypename;
    Side *side2, *side3;

    if (unitbuf == NULL)
      unitbuf = xmalloc(BUFSIZE);
    /* Handle various weird situations. */
    if (unit == NULL)
      return "???";
    if (!alive(unit)) {
    	sprintf(unitbuf, "dead #%d", unit->id);
        return unitbuf;
    }
    /* If this unit represents "yourself", say so. */
    if (side != NULL && unit == side->self_unit)
      return "you";
#if 0 /* how to do these days? */
    /* Sometimes a unit's description should be its name alone. */
    if (u_name_format(unit->type) == 2 && unit->name)
      return unit->name;
#endif
    /* Decide how to identify the side.  If the unit's original side
       is not its current side, list both of them. */
    side2 = unit->side;
    side3 = NULL;
    if (unit->origside != NULL && side2 != unit->origside) {
	side2 = unit->origside;
	side3 = unit->side;
    }
    if (side2 == NULL) {
	sprintf(unitbuf, "the ");
    } else if (side2 == side) {
	sprintf(unitbuf, "your ");
    } else {
	sprintf(unitbuf, "the %s ",
		(side2->adjective ? side2->adjective :
		 (side2->noun ? side2->noun : side_desig(side2))));
    }
    if (side3 != NULL) {
	if (side3 == side) {
	    tprintf(unitbuf, "(your) ");
	} else {
	    tprintf(unitbuf, "(%s-held) ",
		    (side3->adjective ? side3->adjective :
		     (side3->noun ? side3->noun : side_desig(side3))));
	}
    }
    /* Now add the unit's unique description. */
    /* (Should this ever use short name?) */
    utypename = u_type_name(unit->type);
    if (unit->name) {
	tprintf(unitbuf, "%s %s", utypename, unit->name);
    } else if (unit->number > 0) {
	tprintf(unitbuf, "%d%s %s",
		unit->number, ordinal_suffix(unit->number), utypename);
    } else {
	tprintf(unitbuf, "%s", utypename);
    }
    return unitbuf;
}

/* Shorter unit description omits side name, but uses same buffer. */

char *
short_unit_handle(unit)
Unit *unit;
{
    int u;

    if (unitbuf == NULL)
      unitbuf = xmalloc(BUFSIZE);
    if (unit == NULL)
      return "???";
    if (!alive(unit)) {
    	sprintf(unitbuf, "dead #%d", unit->id);
        return unitbuf;
    }
    u = unit->type;
    if (!empty_string(unit->name)) {
	sprintf(unitbuf, "%s", unit->name);
    } else if (!empty_string(u_short_name(u))) {
	sprintf(unitbuf, "%ld%s %s",
		unit->number, ordinal_suffix(unit->number), u_short_name(u));
    } else {
	sprintf(unitbuf, "%ld%s %s",
		unit->number, ordinal_suffix(unit->number), u_type_name(u));
    }
    return unitbuf;
}

void
name_or_number(unit, buf)
Unit *unit;
char *buf;
{
    if (unit->name) {
	sprintf(buf, "%s", unit->name);
    } else if (unit->number > 0) {
	sprintf(buf, "%ld%s", unit->number, ordinal_suffix(unit->number));
    } else {
	buf[0] = '\0';
    }
}

/* Build a short phrase describing a given past unit to a given side,
   basically consisting of indication of unit's side, then of unit itself. */

char *
past_unit_handle(side, past_unit)
Side *side;
PastUnit *past_unit;
{
    char *utypename;
    Side *side2;

    if (past_unitbuf == NULL)
      past_unitbuf = xmalloc(BUFSIZE);
    /* Handle various weird situations. */
    if (past_unit == NULL)
      return "???";
    /* Decide how to identify the side. */
    side2 = past_unit->side;
    if (side2 == NULL) {
	sprintf(past_unitbuf, "the ");
    } else if (side2 == side) {
	sprintf(past_unitbuf, "your ");
    } else {
	sprintf(past_unitbuf, "the %s ",
		(side2->adjective ? side2->adjective :
		 (side2->noun ? side2->noun : side_desig(side2))));
    }
    /* Now add the past_unit's unique description. */
    /* (Should this ever use short name?) */
    utypename = u_type_name(past_unit->type);
    if (past_unit->name) {
	tprintf(past_unitbuf, "%s %s", utypename, past_unit->name);
    } else if (past_unit->number > 0) {
	tprintf(past_unitbuf, "%ld%s %s",
		past_unit->number, ordinal_suffix(past_unit->number), utypename);
    } else {
	tprintf(past_unitbuf, "%s", utypename);
    }
    return past_unitbuf;
}

/* Shorter past_unit description omits side name, but uses same buffer. */

char *
short_past_unit_handle(past_unit)
PastUnit *past_unit;
{
    int u;

    if (past_unitbuf == NULL)
      past_unitbuf = xmalloc(BUFSIZE);
    if (past_unit == NULL)
      return "???";
    u = past_unit->type;
    if (!empty_string(past_unit->name)) {
	sprintf(past_unitbuf, "%s", past_unit->name);
    } else if (!empty_string(u_short_name(u))) {
	sprintf(past_unitbuf, "%ld%s %s",
		past_unit->number, ordinal_suffix(past_unit->number), u_short_name(u));
    } else {
	sprintf(past_unitbuf, "%ld%s %s",
		past_unit->number, ordinal_suffix(past_unit->number), u_type_name(u));
    }
    return past_unitbuf;
}

void
past_name_or_number(past_unit, buf)
PastUnit *past_unit;
char *buf;
{
    if (past_unit->name) {
	sprintf(buf, "%s", past_unit->name);
    } else if (past_unit->number > 0) {
	sprintf(buf, "%ld%s", past_unit->number, ordinal_suffix(past_unit->number));
    } else {
	buf[0] = '\0';
    }
}

/* Given a unit and optional type u, summarize construction status
   and timing. */
/* (should reindent) */
void
construction_desc(buf, unit, u)
char *buf;
Unit *unit;
int u;
{
    int est, u2;
    char ubuf[10], tmpbuf[100];
    Task *task;
    Unit *unit2;

    if (u != NONUTYPE) {
	est = est_completion_time(unit, u);
	if (est >= 0) {
	    sprintf(ubuf, "[%2d] ", est);
	} else {
	    strcpy(ubuf, " --  ");
	}
    } else {
	ubuf[0] = '\0';
    }
    name_or_number(unit, tmpbuf);
    sprintf(buf, "%s%s %s", ubuf, u_type_name(unit->type), tmpbuf);
    pad_out(buf, 25);
    if (unit->plan
	&& unit->plan->tasks) {
      task = unit->plan->tasks;
      if (task->type == TASK_BUILD) {
	u2 = task->args[0];
	tprintf(buf, " %s ", (is_unit_type(u2) ? u_type_name(u2) : "?"));
	unit2 = find_unit(task->args[1]);
	if (in_play(unit2) && unit2->type == u2) {
	    tprintf(buf, "%d/%d done ", unit2->cp, u_cp(unit2->type));
	}
	tprintf(buf, "(%d of %d)", task->args[2] + 1, task->args[3]);
      } else if (task->type == TASK_RESEARCH) {
	u2 = task->args[0];
        if (is_unit_type(u2)) {
		tprintf(buf, " %s tech %d/%d",
			u_type_name(u2), unit->side->tech[u2], task->args[1]);
        }
      }
    }
}

void
constructible_desc(buf, side, u, unit)
char *buf;
Side *side;
int u;
Unit *unit;
{
    char estbuf[20];
    char techbuf[50];
    char typenamebuf[50];
    int est, tp, num;

    if (unit != NULL) {
	est = est_completion_time(unit, u);
    	if (est >= 0) {
	    sprintf(estbuf, "[%2d] ", est);
	    if (uu_tp_to_build(unit->type, u) > 0) {
		tp = (unit->tooling ? unit->tooling[u] : 0);
		tprintf(estbuf, "(%2d) ", tp);
	    }
    	} else {
	    strcpy(estbuf, " --  ");
    	}
    } else {
	estbuf[0] = '\0';
    }
    if (u_tech_max(u) > 0) {
    	sprintf(techbuf, "[T %d/%d/%d] ", side->tech[u], u_tech_to_build(u), u_tech_max(u));
    } else {
	techbuf[0] = '\0';
    }
    strcpy(typenamebuf, u_type_name(u));
    /* If the single char for the type is different from the first character
       of its type name, mention the char. */
    if (!empty_string(u_uchar(u)) && (u_uchar(u))[0] != typenamebuf[0]) {
	tprintf(typenamebuf, "(%c)", (u_uchar(u))[0]);
    }
    sprintf(buf, "%s%s%-16.16s", estbuf, techbuf, typenamebuf);
    num = num_units_in_play(side, u);
    if (num > 0) {
	tprintf(buf, "  %3d", num);
    }
    num = num_units_incomplete(side, u);
    if (num > 0) {
	tprintf(buf, "(%d)", num);
    }
}

int
est_completion_time(unit, u2)
Unit *unit;
int u2;
{
    int u, tooluptime, tp;

    u = unit->type;
    if (uu_acp_to_create(u, u2) < 1)
      return (-1);
    tooluptime = 0;
    tp = (unit->tooling ? unit->tooling[u2] : 0);
    if (tp < uu_tp_to_build(u, u2)) {
	if (uu_acp_to_toolup(u, u2) < 1 || uu_tp_per_toolup(u, u2) <= 0 || u_acp(u) <= 0)
	  return (-1);
	tooluptime = ((uu_tp_to_build(u, u2) - tp) * uu_acp_to_toolup(u, u2))
	 / (uu_tp_per_toolup(u, u2) * u_acp(u));
    }
    return tooluptime + normal_completion_time(unit->type, u2);
}

void
historical_event_date_desc(hevt, buf)
HistEvent *hevt;
char *buf;
{
    sprintf(buf, "%d: ", hevt->startdate);
}

int
find_event_type(sym)
Obj *sym;
{
    int i;

    for (i = 0; hevtdefns[i].name != NULL; ++i) {
	if (strcmp(c_string(sym), hevtdefns[i].name) == 0)
	  return i;
    }
    return -1;
}

void
historical_event_desc(side, hevt, buf)
Side *side;
HistEvent *hevt;
char *buf;
{
    int data0 = hevt->data[0];
    int data1 = hevt->data[1];
    Obj *rest, *head, *pattern, *text;
    Unit *unit;
    PastUnit *pastunit, *pastunit2;
    Side *side2;
    
    for_all_list(g_event_messages(), rest) {
	head = car(rest);
	if (consp(head)) {
	    pattern = car(head);
	    if (symbolp(pattern)
		&& find_event_type(pattern) == hevt->type) {
		text = cadr(head);
		sprintf(buf, c_string(text));
		return;
	    } else if (consp(pattern)
		       && symbolp(car(pattern))
		       && find_event_type(car(pattern)) == hevt->type
		       /* && args match pattern args */
		       ) {
		text = cadr(head);
		sprintf(buf, c_string(text));
		return;
	    }
	}
    }
    /* Generate a default description of the event. */
    switch (hevt->type) {
      case H_LOG_STARTED:
	sprintf(buf, "we started recording events");
	break;
      case H_LOG_ENDED:
	sprintf(buf, "we stopped recording events");
	break;
      case H_GAME_STARTED:
	sprintf(buf, "we started the game");
	break;
      case H_GAME_SAVED:
	sprintf(buf, "we saved the game");
	break;
      case H_GAME_RESTARTED:
	sprintf(buf, "we restarted the game");
	break;
      case H_GAME_ENDED:
	sprintf(buf, "we ended the game");
	break;
      case H_SIDE_JOINED:
      	side2 = side_n(data0);
	sprintf(buf, "%s joined the game",
		(side == side2 ? "you" : side_name(side2)));
	break;
      case H_SIDE_LOST:
      	side2 = side_n(data0);
	sprintf(buf, "%s lost!", (side == side2 ? "you" : side_name(side2)));
	/* Include an explanation of the cause, if there is one. */
	if (data1 == -1) {
	    tprintf(buf, " (resigned)");
	} else if (data1 == -2) {
	    tprintf(buf, " (self-unit died)");
	} else if (data1 > 0) {
	    tprintf(buf, " (scorekeeper %d)", data1);
	} else {
	    tprintf(buf, " (don't know why)");
	}
	break;
      case H_SIDE_WITHDREW:
      	side2 = side_n(data0);
	sprintf(buf, "%s withdrew!", (side == side2 ? "you" : side_name(side2)));
	break;
      case H_SIDE_WON:
      	side2 = side_n(data0);
	sprintf(buf, "%s won!", (side == side2 ? "you" : side_name(side2)));
	/* Include an explanation of the cause, if there is one. */
	if (data1 > 0) {
	    tprintf(buf, " (scorekeeper %d)", data1);
	} else {
	    tprintf(buf, " (don't know why)");
	}
	break;
      case H_UNIT_CREATED:
      	side2 = side_n(data0);
	sprintf(buf, "%s created ",
		(side == side2 ? "you" : side_name(side2)));
	unit = find_unit(data1);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
	    pastunit = find_past_unit(data1);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		tprintf(buf, "%d??", data1);
	    }
	}
	break;
      case H_UNIT_COMPLETED:
      	side2 = side_n(data0);
	sprintf(buf, "%s completed ",
		(side == side2 ? "you" : side_name(side2)));
	unit = find_unit(data1);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
	    pastunit = find_past_unit(data1);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		tprintf(buf, "%d??", data1);
	    }
	}
	break;
      case H_UNIT_DAMAGED:
	unit = find_unit_dead_or_alive(data0);
	if (unit != NULL) {
	    sprintf(buf, "%s", unit_handle(side, unit));
	} else {
 	    pastunit = find_past_unit(data0);
	    if (pastunit != NULL) {
		sprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		sprintf(buf, "%d??", data0);
	    }
	}
	tprintf(buf, " damaged (%d -> %d hp)", data1, hevt->data[2]);
	break;
      case H_UNIT_CAPTURED:
	buf[0] = '\0';
	/* Note that the second optional value, if present, is the id
	   of the unit that did the capturing. */
	unit = find_unit_dead_or_alive(data1);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
 	    pastunit = find_past_unit(data1);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else if (data1 == 0) {
		tprintf(buf, "somebody");
	    } else {
		tprintf(buf, "%d??", data1);
	    }
	}
 	tprintf(buf, " captured ");
 	/* Describe the unit that was captured. */
	unit = find_unit_dead_or_alive(data0);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
 	    pastunit = find_past_unit(data0);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		tprintf(buf, "%d??", data0);
	    }
	}
	break;
      case H_UNIT_ACQUIRED:
	buf[0] = '\0';
	if (data1 >= 0) {
	    side2 = side_n(data1);
	    tprintf(buf, "%s", (side == side2 ? "you" : side_name(side2)));
	} else {
	    tprintf(buf, "somebody");
	}
 	tprintf(buf, " acquired ");
 	/* Describe the unit that was acquired. */
	unit = find_unit_dead_or_alive(data0);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
 	    pastunit = find_past_unit(data0);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		tprintf(buf, "%d??", data0);
	    }
	}
	break;
      case H_UNIT_REVOLTED:
	buf[0] = '\0';
 	/* Describe the unit that revolted. */
	unit = find_unit_dead_or_alive(data0);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
 	    pastunit = find_past_unit(data0);
	    if (pastunit != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit));
	    } else {
		tprintf(buf, "%d??", data0);
	    }
	}
 	tprintf(buf, " revolted");
 	if (data1 >= 0) {
	    side2 = side_n(data1);
	    tprintf(buf, ", went over to %s", (side == side2 ? "you" : side_name(side2)));
	}
	break;
      case H_UNIT_KILLED:
      case H_UNIT_DIED_IN_ACCIDENT:
      case H_UNIT_DIED_FROM_TEMPERATURE:
	/* Obviously, the unit mentioned here can only be a past unit. */
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	if (hevt->type == H_UNIT_KILLED)
	  tprintf(buf, " was destroyed");
	else if (hevt->type == H_UNIT_DIED_IN_ACCIDENT)
	  tprintf(buf, " died in an accident");
	else
	  tprintf(buf, " died from excessive temperature");
	break;
      case H_UNIT_WRECKED:
      case H_UNIT_WRECKED_IN_ACCIDENT:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	if (hevt->type == H_UNIT_WRECKED)
	  tprintf(buf, " was wrecked");
	else
	  tprintf(buf, " was wrecked in an accident");
	break;
      case H_UNIT_VANISHED:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	tprintf(buf, " vanished");
	break;
      case H_UNIT_DISBANDED:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	tprintf(buf, " was disbanded");
	break;
      case H_UNIT_GARRISONED:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	tprintf(buf, " was used to garrison ");
	unit = find_unit(data1);
	if (unit != NULL) {
	    tprintf(buf, "%s", unit_handle(side, unit));
	} else {
	    pastunit2 = find_past_unit(data1);
	    if (pastunit2 != NULL) {
		tprintf(buf, "%s", past_unit_handle(side, pastunit2));
	    } else {
		/* Should never happen, but don't choke if it does. */
		tprintf(buf, "?????");
	    }
	}
	break;
      case H_UNIT_STARVED:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	tprintf(buf, " starved to death");
	break;
      case H_UNIT_LEFT_WORLD:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	tprintf(buf, " left the world");
	break;
      case H_UNIT_NAME_CHANGED:
	pastunit = find_past_unit(data0);
	if (pastunit != NULL) {
	    sprintf(buf, "%s", past_unit_handle(side, pastunit));
	} else {
	    sprintf(buf, "%d??", data0);
	}
	unit = find_unit(data1);
	if (unit != NULL) {
	    if (unit->name != NULL)
	      tprintf(buf, " changed name to \"%s\"", unit->name);
	    else
	      tprintf(buf, " became anonymous");
	} else {
	    pastunit2 = find_past_unit(data1);
	    if (pastunit2 != NULL) {
		if (pastunit2->name != NULL)
		  tprintf(buf, " changed name to \"%s\"", pastunit2->name);
		else
		  tprintf(buf, " became anonymous");
	    }
	}
	break;
      default:
	/* Don't warn, will cause serious problems for windows that
	   are lists of events, but make sure the non-understood event
	   is obvious. */
	sprintf(buf, "?????????? \"%s\" ??????????",
		hevtdefns[hevt->type].name);
	break;
    }
}

char *
action_result_desc(rslt)
int rslt;
{
    char *str;
    
    switch (rslt) {
      case A_ANY_OK:
	str = "OK";
	break;
      case A_ANY_DONE:
	str = "done";
	break;
      case A_ANY_CANNOT_DO:
	str = "can never do";
	break;
      case A_ANY_NO_ACP:
	str = "insufficient acp";
	break;
      case A_ANY_NO_MATERIAL:
	str = "insufficient material";
	break;
      case A_ANY_TOO_FAR:
	str = "too far";
	break;
      case A_ANY_TOO_NEAR:
	str = "too near";
	break;
      case A_MOVE_NO_MP:
	str = "insufficient mp";
	break;
      case A_MOVE_CANNOT_LEAVE_WORLD:
	str = "cannot leave world";
	break;
      case A_MOVE_DEST_FULL:
	str = "destination full";
	break;
      case A_OVERRUN_FAILED:
	str = "overrun failed";
	break;
      case A_OVERRUN_SUCCEEDED:
	str = "overrun succeeded";
	break;
      case A_FIRE_INTO_OUTSIDE_WORLD:
	str = "cannot fire outside world";
	break;
      case A_CAPTURE_FAILED:
	str = "capture failed";
	break;
      case A_CAPTURE_SUCCEEDED:
	str = "capture succeeded";
	break;
      case A_ANY_ERROR:
	str = "misc error";
	break;
      default:
	str = "???";
	break;
    }
    return str;
}

/* Generate a description of the borders and connections in and around
   a location. */

void
linear_desc(buf, x, y)
char *buf;
int x, y;
{
    int t, dir;

    if (any_aux_terrain_defined()) {
	for_all_terrain_types(t) {
	    if (t_is_border(t)
		&& aux_terrain_defined(t)
		&& any_borders_at(x, y, t)) {
		tprintf(buf, " %s", t_type_name(t)); 
		for_all_directions(dir) {
		    if (border_at(x, y, dir, t)) {
			tprintf(buf, "/%s", dirnames[dir]);
		    }
		}
	    }
	    if (t_is_connection(t)
		&& aux_terrain_defined(t)
		&& any_connections_at(x, y, t)) {
		tprintf(buf, " %s", t_type_name(t)); 
		for_all_directions(dir) {
		    if (connection_at(x, y, dir, t)) {
			tprintf(buf, "/%s", dirnames[dir]);
		    }
		}
	    }
	}
    }
}

void
elevation_desc(buf, x, y)
char *buf;
int x, y;
{
    if (elevations_defined()) {
	sprintf(buf, "(Elev %d)", elev_at(x, y));
    }
}

char *
feature_desc(feature, buf)
Feature *feature;
char *buf;
{
    int i, capitalize = FALSE;
    char *str;

    if (feature == NULL)
      return NULL;
    if (feature->name) {
	/* Does the name need any substitutions done? */
	if (strchr(feature->name, '%')) {
	    i = 0;
	    for (str = feature->name; *str != '\0'; ++str) {
	    	if (*str == '%') {
		    /* Interpret substitution directives. */
		    switch (*(str + 1)) {
		      case 'T':
			capitalize = TRUE;
		      case 't':
			if (feature->typename) {
			    buf[i] = '\0';
			    strcat(buf, feature->typename);
			    if (capitalize && islower(buf[i]))
			      buf[i] = toupper(buf[i]);
			    i = strlen(buf);
			} else {
			    /* do nothing */
			}
			++str;
			break;
		      case '\0':
			break;  /* (should be error?) */
		      default:
			/* (error?) */
			break;
		    }
	    	} else {
		    buf[i++] = *str;
	    	}
	    }
	    /* Close off the string. */
	    buf[i] = '\0';
	    return buf;
	} else {
	    /* Return the name alone. */
	    return feature->name;
	}
    } else {
	if (feature->typename) {
	    sprintf(buf, "unnamed %s", feature->typename);
	    return buf;
	}
    }
    /* No description of the location is available. */
    return NULL;
}

/* Generate a string describing what is at the given location. */

char *featurebuf = NULL;

char *
feature_name_at(x, y)
int x, y;
{
    int fid = (features_defined() ? raw_feature_at(x, y) : 0);
    Feature *feature;

    if (fid == 0)
      return NULL;
    feature = find_feature(fid);
    if (feature != NULL) {
	if (featurebuf == NULL)
	  featurebuf = xmalloc(BUFSIZE);
	return feature_desc(feature, featurebuf);
    }
    /* No description of the location is available. */
    return NULL;
}

void
temperature_desc(buf, x, y)
char *buf;
int x, y;
{
    if (temperatures_defined()) {
	sprintf(buf, "(Temp %d)", temperature_at(x, y));
    }
}

#if 0
    int age, u;
    short view, prevview;
    Side *side2;

    /* Compose and display view history of this cell. */
    Dprintf("Drawing previous view info\n");
    age = side_view_age(side, curx, cury);
    prevview = side_prevview(side, curx, cury);
    if (age == 0) {
	if (prevview != view) {
	    if (prevview == EMPTY) {
		/* misleading if prevview was set during init. */
		sprintf(tmpbuf, "Up to date; had been empty.");
	    } else if (prevview == UNSEEN) {
		sprintf(tmpbuf, "Up to date; had been unexplored.");
	    } else {
		side2 = side_n(vside(prevview));
		u = vtype(prevview);
		if (side2 != side) {
		    sprintf(tmpbuf, "Up to date; had seen %s %s.",
			    (side2 == NULL ? "independent" :
			     side_name(side2)),
			    u_type_name(u));
		} else {
		    sprintf(tmpbuf,
			    "Up to date; had been occupied by your %s.",
			    u_type_name(u));
		}
	    }
	} else {
	    sprintf(tmpbuf, "Up to date.");
	}
    } else {
	if (prevview == EMPTY) {
	    sprintf(tmpbuf, "Was empty %d turns ago.", age);
	} else if (prevview == UNSEEN) {
	    sprintf(tmpbuf, "Terrain first seen %d turns ago.", age);
	} else {
	    side2 = side_n(vside(prevview));
	    u = vtype(prevview);
	    if (side2 != side) {
		sprintf(tmpbuf, "Saw %s %s, %d turns ago.",
			(side2 == NULL ? "independent" :
			 side_name(side2)),
			u_type_name(u), age);
	    } else {
		sprintf(tmpbuf, "Was occupied by your %s %d turns ago.",
			u_type_name(u), age);
	    }
	}
    }
#endif

void
hp_desc(buf, unit, label)
char *buf;
Unit *unit;
int label;
{
    if (label) {
	sprintf(buf, "HP ");
    } else {
	buf[0] = '\0';
    }
    /* (print '-' or some such for zero hp case?) */
    if (unit->hp == u_hp(unit->type)) {
	tprintf(buf, "%d", unit->hp);
    } else {
	tprintf(buf, "%d/%d", unit->hp, u_hp(unit->type));
    } 
}

void
acp_desc(buf, unit, label)
char *buf;
Unit *unit;
int label;
{
    int u = unit->type;

    if (!completed(unit)) {
	sprintf(buf, "%d/%d done", unit->cp, u_cp(u));
    } else if (unit->act && u_acp(u) > 0) {
    	if (label) {
    	    strcpy(buf, "ACP ");
    	} else {
	    buf[0] = '\0';
    	}
	if (unit->act->acp == unit->act->initacp) {
	    tprintf(buf, "%d", unit->act->acp);
	} else {
	    tprintf(buf, "%d/%d", unit->act->acp, unit->act->initacp);
	}
    } else {
	buf[0] = '\0';
    }
}

int
supply_desc(buf, unit, mrow)
char *buf;
Unit *unit;
int mrow;
{
    int u = unit->type, m, mm, tmprow;

    tmprow = 0;
    buf[0] = '\0';
    mm = 0;
    for_all_material_types(m) {
	if (um_storage_x(u, m) > 0) {
	    if (mm > 0 && mm % 3 == 0)
	      ++tmprow;
	    if (tmprow == mrow) {
		tprintf(buf, "%s %d/%d  ",
			m_type_name(m), unit->supply[m], um_storage_x(u, m));
	    }
	    ++mm;
	}
    }
    return (strlen(buf) > 0);
}

void
location_desc(buf, side, unit, u, x, y)
char *buf;
Side *side;
Unit *unit;
int u, x, y;
{
    int t = terrain_at(x, y);
    char *featurename;

    if (unit != NULL && unit->transport != NULL) {
	sprintf(buf, "In %s", short_unit_handle(unit->transport));
    } else if (unit != NULL || u != NONUTYPE) {
	sprintf(buf, "In %s", t_type_name(t));
    } else if (terrain_visible(side, x, y)) {
	sprintf(buf, "Empty %s", t_type_name(t));
    } else {
	sprintf(buf, "Unknown");
    }
    if (terrain_visible(side, x, y)) {
	linear_desc(buf, x, y);
	/* (should use feature desc?) */
	featurename = feature_name_at(x, y);
	if (!empty_string(featurename))
	  tprintf(buf, " (%s)", featurename);
	if (temperatures_defined())
	  tprintf(buf, " (T %d)", temperature_at(x, y));
	if (elevations_defined())
	  tprintf(buf, " (El %d)", elev_at(x, y));
	/* (should list local weather also) */
    }
    tprintf(buf, " at %d,%d", x, y);
}

void
plan_desc(buf, unit)
char *buf;
Unit *unit;
{
    int i;
    Plan *plan = unit->plan;
    Task *task = plan->tasks;

    if (plan == NULL) {
	buf[0] = '\0';
    	return;
    }
    sprintf(buf, "%s", plantypenames[plan->type]);
    if (plan->waitingfortasks)
      strcat(buf, " Wait");
    if (plan->asleep)
      strcat(buf, " Asleep");
    if (plan->reserve)
      strcat(buf, " Reserve");
    if (plan->delayed)
      strcat(buf, " Delay");
    if (plan->aicontrol)
      strcat(buf, " AI");
    if (plan->supply_is_low)
      strcat(buf, " Low");
    if (plan->supply_alarm)
      strcat(buf, " SupplyAlarm");
    if (plan->maingoal)
      strcat(buf, " <goal>");
    if (plan->formation)
      strcat(buf, " <formation>");
    if (plan->tasks) {
	i = 0;
	for_all_tasks(plan, task)
	  ++i;
	tprintf(buf, " %d task", i);
	if (i > 1)
	  strcat(buf, "s");
    } 
}

void
task_desc(buf, task)
char *buf;
Task *task;
{
    int i, slen;
    char *argtypes;

    sprintf(buf, "%s", taskdefns[task->type].name);
    switch (task->type) {
      case TASK_BUILD:
	tprintf(buf, " %s", u_type_name(task->args[0]));
	if (task->args[1] != 0) {
	    Unit *unit = find_unit(task->args[1]);

	    if (unit != NULL) {
		tprintf(buf, " (%d cp)", unit->cp);
	    }
	}
	tprintf(buf, ", %d of %d", task->args[2], task->args[3]);
	break;
      case TASK_HIT_POSITION:
      	tprintf(buf, " %d,%d", task->args[0], task->args[1]);
	break;
      case TASK_HIT_UNIT:
      	tprintf(buf, " at %d,%d (type %d side %d)",
      		task->args[0], task->args[1], task->args[2], task->args[3]);
	break;
      case TASK_MOVE_TO:
        if (task->args[2] == 0) {
	    tprintf(buf, " %d,%d", task->args[0], task->args[1]);
        } else if (task->args[2] == 1) {
	    tprintf(buf, " adjacent %d,%d", task->args[0], task->args[1]);
        } else {
	    tprintf(buf, " within %d of %d,%d",
		    task->args[2], task->args[0], task->args[1]);
        }
	break;
      default:
	argtypes = taskdefns[task->type].argtypes;
	slen = strlen(argtypes);
	for (i = 0; i < slen; ++i) {
	    tprintf(buf, "%c%d", (i == 0 ? ' ' : ','), task->args[i]);
	}
	break;
    }
    tprintf(buf, " x %d", task->execnum);
    if (task->retrynum > 0) {
	tprintf(buf, " fail %d", task->retrynum);
    }
}

/* Format a clock time into a standard form.  This routine will omit the hours
   part if it will be uninteresting. */

void
time_desc(buf, time, maxtime)
char *buf;
int time, maxtime;
{
    int hour, minute, second;

    if (time >= 0) {
	hour = time / 3600;  minute = (time / 60) % 60;  second = time % 60;
    	if (between(1, maxtime, 3600) && hour == 0) {
	    sprintf(buf, "%.2d:%.2d", minute, second);
	} else {
	    sprintf(buf, "%.2d:%.2d:%.2d", hour, minute, second);
	}
    } else {
    	sprintf(buf, "??:??:??");
    }
}

/* General-purpose routine to take an array of anonymous unit types and
   summarize what's in it, using numbers and unit chars. */

char *
summarize_units(buf, ucnts)
char *buf;
int *ucnts;
{
    char tmp[BUFSIZE];  /* should be bigger? */
    int u;

    buf[0] = '\0';
    for_all_unit_types(u) {
	if (ucnts[u] > 0) {
	    sprintf(tmp, " %d %s", ucnts[u], utype_name_n(u, 3));
	    strcat(buf, tmp);
	}
    }
    return buf;
}

/* Compose a one-line comment on the game. */

char *
exit_commentary(side)
Side *side;
{
    int numingame = 0, numwon = 0, numlost = 0;
    Side *side2, *lastside, *winner, *loser;

    for_all_sides(side2) {
	if (side2->ingame) {
	    ++numingame;
	    lastside = side2;
	}
	if (side_won(side2)) {
	    ++numwon;
	    winner = side2;
	}
	if (side_lost(side2)) {
	    ++numlost;
	    loser = side2;
	}
    }
    if (numingame > 0) {
    	if (0 /* could have been resolved, need to check scorekeepers */) {
	    sprintf(spbuf, "The outcome remains undecided");
#ifdef RUDE
	    if (numsides > 1) {
		strcat(spbuf, ", but you're probably the loser!");
	    } else {
		strcat(spbuf, "...");
	    }
#else
	    strcat(spbuf, "...");
#endif /* RUDE */
	} else {
	    sprintf(spbuf, "Game is over.");
    	}
    } else if (numwon == numsides) {
	sprintf(spbuf, "Everybody won!");
    } else if (numlost == numsides) {
	sprintf(spbuf, "Everybody lost!");
    } else if (numwon == 1) {
	sprintf(spbuf, "%s won!", (side == winner ? "You" : side_desig(winner)));
    } else if (numlost == 1) {
	sprintf(spbuf, "%s lost!", (side == loser ? "You" : side_desig(loser)));
    } else {
	sprintf(spbuf, "Some won and some lost.");
    }
    return spbuf;
}

void
notify_all_of_resignation(side, side2)
Side *side, *side2;
{
    Side *side3;

    for_all_sides(side3) {
	if (side3 != side) {
	    notify(side3,
		   "%s %s giving up!",
		   short_side_title_with_adjective(side,
#ifdef RUDE
		   (flip_coin() ? "cowardly" : "wimpy")
#else
		   NULL
#endif /* RUDE */
		   ),
		   (short_side_title_plural_p(side) ? "are" : "is"));
	    if (side2 != NULL) {
		notify(side3, "... and donating everything to %s!",
		       short_side_title(side2));
	    }
	}
    }
}

/* The following routines should probably go into some kind of international
   type interface. */

/* Given a number, figure out what suffix should go with it. */

char *
ordinal_suffix(n)
int n;
{
    if (n % 100 == 11 || n % 100 == 12 || n % 100 == 13) {
	return "th";
    } else {
	switch (n % 10) {
	  case 1:   return "st";
	  case 2:   return "nd";
	  case 3:   return "rd";
	  default:  return "th";
	}
    }
}

/* Pluralize a word, attempting to be smart about various possibilities
   that don't have a different plural form (such as "Chinese" and "Swiss"). */

/* There should probably be a test for when to add "es" instead of "s". */

char *
plural_form(word)
char *word;
{
    char endch = ' ', nextend = ' ';

    if (word == NULL) {
	run_warning("plural_form given NULL string");
	pluralbuf[0] = '\0';
	return pluralbuf;
    }
    if (strlen(word) > 0)
      endch   = word[strlen(word)-1];
    if (strlen(word) > 1)
      nextend = word[strlen(word)-2];
    if (endch == 'h' || endch == 's' || (endch == 'e' && nextend == 's')) {
	sprintf(pluralbuf, "%s", word);
    } else {
	sprintf(pluralbuf, "%ss", word);
    }
    return pluralbuf;
}

/* General text generation. */

char *
make_text(buf, maker, a1, a2, a3, a4)
char *buf;
Obj *maker;
long a1, a2, a3, a4;
{
    if (buf == NULL)
      buf = xmalloc(BUFSIZE);
    if (maker != lispnil) {
    } else {
	sprintf(buf, "%ld %ld %ld %ld", a1, a2, a3, a4);
    }
    return buf;
}

/* Date/time parsing and formatting. */

long
parse_date(str)
char *str;
{
    return 1;
}

/* Compose a readable form of the given date. */

char *
absolute_date_string(date)
int date;
{
    /* The first time we ask for a date, interpret the calendar. */
    if (calendar_type == cal_unknown)
      init_calendar();
    switch (calendar_type) {
      case cal_turn:
	sprintf(datebuf, "%s%4ld", turnname, date);
	return datebuf;
      case cal_usual:
	return usual_date_string(date);
      default:
	case_panic("calendar type", calendar_type);
    }
    return "!?!";
}

/* Interpret the calendar definition. */

static void
init_calendar()
{
    int caltypeunknown = FALSE;
    char *stepnamestr;
    Obj *cal, *caltype, *stepname, *step;

    cal = g_calendar();
    /* Turn-based calendar is the default. */
    calendar_type = cal_turn;
    turnname = "Turn";
    if (cal == lispnil) {
	/* Default is fine. */
    } else if (consp(cal)) {
	caltype = car(cal);
	if ((symbolp(caltype) || stringp(caltype))
	     && strcmp("usual", c_string(caltype)) == 0) {
	    calendar_type = cal_usual;
	    stepname = cadr(cal);
	    if (symbolp(stepname) || stringp(stepname)) {
		stepnamestr = c_string(stepname);
		    if (strcmp(stepnamestr, "second") == 0) {
			datesteptype = ds_second;
		    } else if (strcmp(stepnamestr, "minute") == 0) {
			datesteptype = ds_minute;
		    } else if (strcmp(stepnamestr, "hour") == 0) {
			datesteptype = ds_hour;
		    } else if (strcmp(stepnamestr, "day") == 0) {
			datesteptype = ds_day;
		    } else if (strcmp(stepnamestr, "week") == 0) {
			datesteptype = ds_week;
		    } else if (strcmp(stepnamestr, "month") == 0) {
			datesteptype = ds_month;
		    } else if (strcmp(stepnamestr, "season") == 0) {
			datesteptype = ds_season;
		    } else if (strcmp(stepnamestr, "year") == 0) {
			datesteptype = ds_year;
		    } else {
			init_warning("\"%s\" not a known date step name", stepnamestr);
		    }
	    } else {
		init_warning("No name for date step type, substituting `day'");
		datesteptype = ds_day;
	    }
	    /* Collect an optional multiple. */
	    step = car(cddr(cal));
	    datestep = (numberp(step) ? c_number(step) : 1);
	    if (!empty_string(g_initial_date())) {
		set_initial_date(g_initial_date());
	    }
	} else {
	    caltypeunknown = TRUE;
	}
    } else if (stringp(cal)) {
	turnname = c_string(cal);
    } else {
	caltypeunknown = TRUE;
    }
    if (caltypeunknown)
      init_warning("Unknown calendar type");
}

/* Compose a date, omitting components supplied by the base date. */

char *
relative_date_string(date, base)
int date, base;
{
    /* should do this for real eventually */
    sprintf(datebuf, "%ld(%ld)", date, base);
    return datebuf;
}

extern int turns_between PARAMS ((char *str1, char *str2));

int
turns_between(datestr1, datestr2)
char *datestr1, *datestr2;
{
    int rslt;
    UsualDate date1, date2;

    if (calendar_type == cal_unknown)
      init_calendar();
    switch (calendar_type) {
      case cal_usual:
	parse_usual_date(datestr1, &date1);
	parse_usual_date(datestr2, &date2);
	rslt = date2.year - date1.year;
	if (datesteptype == ds_year)
	  return (rslt / datestep);
	if (datesteptype < ds_year) {
	    rslt = (12 * rslt) - (date2.month - date1.month);
	}
	if (datesteptype == ds_month)
	  return (rslt / datestep);
	if (datesteptype < ds_month) {
	    rslt = (30 * rslt) - (date2.day - date1.day);
	}
	if (datesteptype == ds_week)
	  return (((rslt + 6) / 7) / datestep);
	if (datesteptype == ds_day)
	  return (rslt / datestep);
	if (datesteptype < ds_day) {
	    rslt = (24 * rslt) - (date2.hour - date1.hour);
	}
	if (datesteptype == ds_hour)
	  return (rslt / datestep);
	return (rslt / datestep); /* semi-bogus */
      default:
	return 1;
    }
}

void
set_initial_date(str)
char *str;
{
    if (calendar_type == cal_unknown)
      init_calendar();
    switch (calendar_type) {
      case cal_usual:
	if (usual_initial == NULL)
	  usual_initial = (UsualDate *) xmalloc(sizeof(UsualDate));
	parse_usual_date(str, usual_initial);
	break;
      default:
	break;
    }
}

static void
parse_usual_date(datestr, udate)
char *datestr;
UsualDate *udate;
{
    char monthname[BUFSIZE];
    int i, cnt;
    
    udate->second = udate->minute = udate->hour = udate->day = udate->month = udate->year = 0;
    monthname[0] = '\0';
    if (!empty_string(datestr)) {
	/* Assume it's in a standard date format. */
	switch (datesteptype) {
	  case ds_hour:
	    cnt = sscanf(datestr, "%d:00 %d %s %d",
			 &(udate->hour), &(udate->day), monthname,
			 &(udate->year));
	    if (cnt != 4)
	      cnt = sscanf(datestr, "%d %d %s %d",
			   &(udate->hour), &(udate->day), monthname,
			   &(udate->year));
	    if (cnt != 4)
	      goto bad_format;
	    --(udate->day);
	    break;
	  case ds_day:
	    cnt = sscanf(datestr, "%d %s %d", &(udate->day), monthname,
			 &(udate->year));
	    if (cnt != 3)
	      goto bad_format;
	    --(udate->day);
	    break;
	  case ds_week:
	    cnt = sscanf(datestr, "%d %s %d", &(udate->day), monthname,
			 &(udate->year));
	    if (cnt != 3)
	      goto bad_format;
	    --(udate->day);
	    break;
	  case ds_month:
	    cnt = sscanf(datestr, "%s %d", monthname, &(udate->year));
	    if (cnt != 2)
	      goto bad_format;
	    break;
	  case ds_year:
	    cnt = sscanf(datestr, "%d", &(udate->year));
	    if (cnt != 2)
	      goto bad_format;
	    return;
	  default:
	    init_warning("%d not a known date step type", datesteptype);
	    break;
	}
	for (i = 0; i < 12; ++i) {
	    if (strcmp(monthname, months[i]) == 0) {
		udate->month = i;
		return;
	    }
	}
	init_warning("\"%s\" not a recognized month name", monthname);
    }
    return;
  bad_format:
    init_warning("\"%s\" is a badly formatted date", datestr);
}


/* Given a numeric data, convert it into something understandable.  Depends
   on the length of a turn. */

static char *
usual_date_string(date)
int date;
{
    int year = 0, season = 0, month = 0, day = 0;
    int hour = 0, second = 0, minute = 0;

    /* The date, which is a turn number, should be 1 or more, but this
       routine may be called before the game really starts, so return
       something that will be distinctive if it's ever displayed. */
    if (date <= 0)
      return "pregame";
    /* First displayed date is normally turn 1; be zero-based for
       the benefit of calculation. */
    --date;
    date *= datestep;
    if (usual_initial == NULL) {
	usual_initial = (UsualDate *) xmalloc(sizeof(UsualDate));
	if (!empty_string(g_initial_date()))
	  parse_usual_date(g_initial_date(), usual_initial);
    }
    switch (datesteptype) {
      case ds_second:
	second = date % 60;
	minute = date / 60;
	sprintf(datebuf, "%d:%d", minute, second);
	break;
      case ds_minute:
	minute = date % 60;
	hour = date / 60 + usual_initial->hour;
	sprintf(datebuf, "%d:%d", hour, minute);
	break;
      case ds_hour:
 	hour = (date + usual_initial->hour) % 24;
	/* Convert to days, then proceed as for days. */
 	date = (date + usual_initial->hour) / 24;
	date += 30 * usual_initial->month;
	day = date % 365;
	month = day / 30;
	day = (day + usual_initial->day) % 30 + 1;
	year = date / 365 + usual_initial->year;
	sprintf(datebuf, "%d:00 %2d %s %d", hour, day, months[month], ABS(year));
	break;
      case ds_day:
	/* Should do this more accurately... */
	date += 30 * usual_initial->month;
	day = date % 365;
	month = day / 30;
	day = (day + usual_initial->day) % 30 + 1;
	year = date / 365 + usual_initial->year;
	sprintf(datebuf, "%2d %s %d", day, months[month], ABS(year));
	break;
      case ds_week:
	/* Convert to days, then proceed as for days. */
	date *= 7;
	date += 30 * usual_initial->month;
	day = date % 365;
	month = day / 30;
	day = (day + usual_initial->day) % 30 + 1;
	year = date / 365 + usual_initial->year;
	sprintf(datebuf, "%2d %s %d", day, months[month], ABS(year));
	break;
      case ds_month:
	date += usual_initial->month;
    	month = date % 12;
	year = date / 12 + usual_initial->year;
	sprintf(datebuf, "%s %d", months[month], ABS(year));
	break;
      case ds_season:
     	season = date % 4;
	year = date / 4 + usual_initial->year;
	sprintf(datebuf, "%d %d", season, ABS(year));
	break;
      case ds_year:
	year = date + usual_initial->year;
	sprintf(datebuf, "%d", ABS(year));
	break;
      default:
	sprintf(datebuf, "?what's a %d?", datesteptype);
	break;
    }
    if (year < 0) {
	strcat(datebuf, " BC");
    }
    return datebuf;
}

/* Show some overall numbers on performance of a side. */

void
write_side_results(fp, side)
FILE *fp;
Side *side;
{
    int i;

    if (side == NULL) {
	fprintf(fp, "Results for game as a whole:\n\n");
    } else {
	fprintf(fp, "Results for %s%s",
		short_side_title(side),
		(side_won(side) ? " (WINNER)" :
		 (side_lost(side) ? " (LOSER)" :
		  "")));
	for (i = 0; i < numscores; ++i) {
	    fprintf(fp, " %d", side->scores[i]);
	}
	fprintf(fp, ", played by %s:\n\n",
		long_player_title(spbuf, side->player, NULL));
    }
}

/* Display what is essentially a double-column bookkeeping of unit gains
   and losses. */

void
write_unit_record(fp, side)
FILE *fp;
Side *side;
{
    int u, gainreason, lossreason, totgain, totloss, val;

    fprintf(fp, "Unit Record (gains and losses by cause and unit type)\n");
    fprintf(fp, " Unit Type ");
    for (gainreason = 0; gainreason < num_gain_reasons; ++gainreason) {
	fprintf(fp, " %3s", gain_reason_names[gainreason]);
    }
    fprintf(fp, " Gain |");
    for (lossreason = 0; lossreason < num_loss_reasons; ++lossreason) {
	fprintf(fp, " %3s", loss_reason_names[lossreason]);
    }
    fprintf(fp, " Loss |");
    fprintf(fp, " Total\n");
    for_all_unit_types(u) {
	if (u_possible[u]) {
	    totgain = 0;
	    fprintf(fp, " %9s ", utype_name_n(u, 9));
	    for (gainreason = 0; gainreason < num_gain_reasons; ++gainreason) {
		val = gain_count(side, u, gainreason);
		if (val > 0) {
		    fprintf(fp, " %3d", val);
		    totgain += val;
		} else {
		    fprintf(fp, "    ");
		}
	    }
	    fprintf(fp, "  %3d |", totgain);
	    totloss = 0;
	    for (lossreason = 0; lossreason < num_loss_reasons; ++lossreason) {
		val = loss_count(side, u, lossreason);
		if (val > 0) {
		    fprintf(fp, " %3d", val);
		    totloss += val;
		} else {
		    fprintf(fp, "    ");
		}
	    }
	    fprintf(fp, "  %3d |", totloss);
	    fprintf(fp, "  %3d\n", totgain - totloss);
	}
    }
    fprintf(fp, "\n");
}

static int
gain_count(side, u, r)
Side *side;
int u, r;
{
    int sum;

    if (side != NULL)
      return side_gain_count(side, u, r);
    sum = 0;
    for_all_sides(side) {
	sum += side_gain_count(side, u, r);
    }
    return sum;
}

static int
loss_count(side, u, r)
Side *side;
int u, r;
{
    int sum;

    if (side != NULL)
      return side_loss_count(side, u, r);
    sum = 0;
    for_all_sides(side) {
	sum += side_loss_count(side, u, r);
    }
    return sum;
}

/* Nearly-raw combat statistics; hard to interpret, but they provide
   a useful check against subjective evaluation of performance. */

void
write_combat_results(fp, side)
FILE *fp;
Side *side;
{
    int a, d, atk;

    fprintf(fp,
	    "Unit Combat Results (average damage over # attacks against enemy, by type)\n");
    fprintf(fp, " A  D->");
    for_all_unit_types(d) {
	if (u_possible[d]) {
	    fprintf(fp, " %4s ", utype_name_n(d, 4));
	}
    }
    fprintf(fp, "\n");
    for_all_unit_types(a) {
	if (u_possible[a]) {
	    fprintf(fp, " %4s ", utype_name_n(a, 4));
	    for_all_unit_types(d) {
		if (u_possible[d]) {
		    atk = atkstats(side, a, d);
		    if (atk > 0) {
			fprintf(fp, " %5.2f",
				((float) hitstats(side, a, d)) / atk);
		    } else {
			fprintf(fp, "      ");
		    }
		}
	    }
	    fprintf(fp, "\n     ");
	    for_all_unit_types(d) {
		if (u_possible[d]) {
		    atk = atkstats(side, a, d);
		    if (atk > 0) {
			fprintf(fp, " %4d ", atk);
		    } else {
			fprintf(fp, "      ");
		    }
		}
	    }
	    fprintf(fp, "\n");
	}
    }
    fprintf(fp, "\n");
}

static int
atkstats(side, a, d)
Side *side;
int a, d;
{
    int sum;

    if (side != NULL)
      return side_atkstats(side, a, d);
    sum = 0;
    for_all_sides(side) {
	sum += side_atkstats(side, a, d);
    }
    return sum;
}

static int
hitstats(side, a, d)
Side *side;
int a, d;
{
    int sum;

    if (side != NULL)
      return side_hitstats(side, a, d);
    sum = 0;
    for_all_sides(side) {
	sum += side_hitstats(side, a, d);
    }
    return sum;
}

void
dice_desc(buf, dice)
char *buf;
int dice;
{
    int numdice, die, offset;

    if (dice >> 14 == 0 || dice >> 14 == 3) {
	sprintf(buf, "%d", dice);
    } else {
    	numdice = (dice >> 11) & 0x07;
    	die = (dice >> 7) & 0x0f;
    	offset = dice & 0x7f;
    	if (offset == 0) {
	    sprintf(buf, "%dd%d", numdice, die);
    	} else {
	    sprintf(buf, "%dd%d+%d", numdice, die, offset);
    	}
    }
}

/* The following code formats a list of types that are missing images. */

void
record_missing_image(typtyp, str)
int typtyp;
char *str;
{
    if (missinglist == NULL) {
	missinglist = xmalloc(BUFSIZE);
	missinglist[0] = '\0';
    }
    ++missing[typtyp];
    /* Add the name of the image-less type, but only if one of
       the first few. */
    if (between(1, totlisted, NUMTOLIST))
      strcat(missinglist, ",");
    if (totlisted < NUMTOLIST) {
	strcat(missinglist, str);
    } else if (totlisted == NUMTOLIST) {
	strcat(missinglist, "...");
    }
    ++totlisted;
}

/* Return true if any images could not be found, and provide some helpful info
   into the supplied buffer. */

int
missing_images(buf)
char *buf;
{
    if (missinglist == NULL)
      return FALSE;
    buf[0] = '\0';
    if (missing[UTYP] > 0)
      tprintf(buf, " %d unit images", missing[UTYP]);
    if (missing[TTYP] > 0)
      tprintf(buf, " %d terrain images", missing[TTYP]);
    if (missing[3] > 0)
      tprintf(buf, " %d emblems", missing[3]);
    tprintf(buf, " - %s", missinglist);
    return TRUE;
}

