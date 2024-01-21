/* Definitions relating to scorekeepers in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

typedef struct a_scorekeeper {
    short id;                   /* unique id number */
    char *title;                /* title by which this is displayed */
    Obj *when;                  /* times at which this will run */
    Obj *who;                   /* which sides this applies to */
    SideMask whomask;		/* mask of sides this applies to */
    Obj *knownto;               /* which sides know about this scorekeeper */
    int initial;               /* initial value of a numeric score */
    Obj *trigger;               /* test that decides triggering */
    short triggered;            /* true when scorekeeper has been triggered */
    Obj *body;                  /* the actual effect-causing stuff */
    Obj *messages;              /* messages to display */
    Obj *record;                /* how to record this in scorefile */
    Obj *notes;                 /* random notes about the scorekeeper */
    short scorenum;             /* index of this scorekeeper's score value */
    struct a_scorekeeper *next; /* pointer to the next scorekeeper */
} Scorekeeper;

/* Iteration over all scorekeepers. */

#define for_all_scorekeepers(sk)  \
  for ((sk) = scorekeepers;  (sk) != NULL;  (sk) = (sk)->next)

#ifdef DESIGNERS
#define keeping_score() (numscorekeepers > 0 && numdesigners == 0)
#else
#define keeping_score() (numscorekeepers > 0)
#endif

#define recording_scores() (0)

extern Scorekeeper *scorekeepers;

extern int numscorekeepers;

extern int numscores;

extern int any_post_action_scores;
extern int any_post_event_scores;

extern void init_scorekeepers PARAMS ((void));
extern Scorekeeper *create_scorekeeper PARAMS ((void));
extern Scorekeeper *find_scorekeeper PARAMS ((int id));
extern void init_scores PARAMS ((void));
extern void check_pre_turn_scores PARAMS ((void));
extern void check_post_turn_scores PARAMS ((void));
extern void check_post_action_scores PARAMS ((Unit *unit, Action *action, int rslt));
extern void check_post_event_scores PARAMS ((HistEvent *hevt));
extern void run_scorekeeper PARAMS ((Side *side, Scorekeeper *sk));
extern int eval_sk_form PARAMS ((Side *side, Scorekeeper *sk, Obj *form));
extern int sum_property PARAMS ((Side *side, Obj *form));
extern int point_value PARAMS ((Unit *unit));
extern int side_point_value PARAMS ((Side *side));
extern int eval_sk_test PARAMS ((Side *side, Scorekeeper *sk, Obj *form));
extern void side_wins PARAMS ((Side *side, int why));
extern void side_loses PARAMS ((Side *side, Side *side2, int why));
extern void all_sides_draw PARAMS ((void));
extern void record_into_scorefile PARAMS ((void));
extern int should_try_to_win PARAMS ((Side *side));

