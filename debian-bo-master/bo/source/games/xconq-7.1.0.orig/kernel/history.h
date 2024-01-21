/* Definitions for the historical record.
   Copyright (C) 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

typedef enum {

#undef  DEF_HEVT
#define DEF_HEVT(name, CODE, datadescs) CODE,

#include "history.def"

    NUMHEVTTYPES
} HistEventType;

/* This is the form of the definition of a event type. */

typedef struct a_hevt_defn {
    char *name;
    char *datadescs;
} HevtDefn;

typedef struct a_histevent {
    HistEventType type;		/* type of historical event */
    short startdate;		/* date of event's start */
    short enddate;		/* date of event's end */
    SideMask observers;		/* which sides know about this event */
    struct a_histevent *next;	/* link to next in list */
    struct a_histevent *prev;	/* link to previous in list */
    short data[4];		/* data describing event */
} HistEvent;

/* This is a snapshot of key bits of a unit's state at a particular
   moment. */

typedef struct a_pastunit {
    short type;			/* type */
    short id;			/* truly unique id number */
    char *name;			/* the name, if given */
    int number;			/* semi-unique number */
    short x, y, z;		/* position of unit in world */
    struct a_side *side;	/* whose side this unit is on */
    struct a_pastunit *next;	/* pointer to next in list */
} PastUnit;

enum gain_reasons {
    initial_gain = 0,
    build_gain = 1,
    capture_gain = 2,
    other_gain = 3,
    num_gain_reasons = 4
};

enum loss_reasons {
    combat_loss = 0,
    capture_loss = 1,
    starvation_loss = 2,
    accident_loss = 3,
    disband_loss = 4,
    other_loss = 5,
    num_loss_reasons = 6
};

enum damage_reasons {
    combat_dmg,
    accident_dmg
};

extern HevtDefn hevtdefns[];

extern HistEvent *history;

extern PastUnit *past_unit_list;

extern void init_history PARAMS ((void));
extern void start_history PARAMS ((void));
extern HistEvent *create_historical_event PARAMS ((HistEventType type));
extern HistEvent *record_event PARAMS ((HistEventType type, SideMask observers, ...));
extern void record_unit_death PARAMS ((Unit *unit, HistEventType reason));
extern void record_unit_name_change PARAMS ((Unit *unit, char *newname));
extern void record_unit_side_change PARAMS ((Unit *unit, Side *newside, HistEventType reason, Unit *agent));
extern void count_gain PARAMS ((Side *side, int u, enum gain_reasons reason));
extern void count_loss PARAMS ((Side *side, int u, enum loss_reasons reason));
extern void end_history PARAMS ((void));
extern HistEvent *get_nth_history_line PARAMS ((Side *side, int n, HistEvent **nextevt));
extern PastUnit *create_past_unit PARAMS ((int type, int id));
extern PastUnit *find_past_unit PARAMS ((int n));
extern char *past_unit_desig PARAMS ((PastUnit *pastunit));
extern PastUnit *change_unit_to_past_unit PARAMS ((Unit *unit));
extern void dump_statistics PARAMS ((void));

