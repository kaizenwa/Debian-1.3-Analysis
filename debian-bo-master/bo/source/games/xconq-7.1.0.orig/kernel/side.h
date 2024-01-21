/* Definitions for sides in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* A side mask is a bit vector, where the bit position corresponds to the
   side number. */

#if MAXSIDES < 31
typedef int SideMask;
#define NOSIDES (0)
#define ALLSIDES (-1)
#define add_side_to_set(side,mask) ((mask) | (1 << ((side) ? (side)->id : 0)))
#define side_in_set(side,mask) ((mask) & (1 << ((side) ? (side)->id : 0)))
#else
not implemented yet
#endif /* MAXSIDES */

/* Doctrine is policy info that units and players use to help decide
   behavior. */

typedef struct a_doctrine {
    short id;			/* a unique id */
    char *name;			/* a distinguishing name for the doctrine */
    short everaskside;		/* should this unit ever ask for orders? */
    short *construction_run;
    short locked;
    struct a_doctrine *next;
} Doctrine;

typedef struct a_standing_order {
    char *types;		/* unit types to which order applies */
    char etype;			/* type of event to trigger on */
    int a1, a2, a3;		/* parameters to test (x, y, etc) */
    struct a_task *task;	/* task to perform */
    struct a_standing_order *next;
} StandingOrder;

/* Each Xconq player is a "side" - more or less one country.  A side may or
   may not be played by a person, and may or may not have a display attached
   to it.  Each side has a different view of the world.  */

typedef struct a_side {
    int id;			/* a unique id */
    Obj *symbol;		/* a symbol bound to side's id */
    /* Name-related slots. */
    char *name;			/* proper name of this side */
    char *longname;		/* the long name of this side */
    char *shortname;		/* the long name of this side */
    char *noun;			/* the noun describing a member of this side */
    char *pluralnoun;		/* the noun describing several members */
    char *adjective;		/* adjective for members of this side */
    char *colorscheme;		/* names of the side's characteristic colors */
    char *emblemname;		/* name of the side's emblem */
    char **unitnamers;
    Obj *featurenamers;
    short nameslocked;		/* true if names may not be changed by player */
    char *sideclass;		/* general type or class description */
    /* Relationships with other sides, types, etc. */
    struct a_unit *self_unit;	/* unit that embodies side */
    short self_unit_id;		/* id of unit that embodies side */
    struct a_side *controlled_by; /* side controlling this one */
    short controlled_by_id;	/* id of controlling side */
    SideMask knows_about;	/* true if side knows about another side */
    short *trusts;		/* true if side trusts another side */
    short *trades;
    short *startwith;		/* how many units of each type at start of game */
    short *counts;		/* array of numbers for identifying units */
    short *tech;		/* tech level for each unit type */
    short *inittech;		/* tech level at beginning of turn */
    Doctrine *default_doctrine;	/* fallback doctrine */
    Doctrine **udoctrine;	/* array of per-unit-type doctrines */
    short doctrines_locked;
    StandingOrder *orders;	/* list of standing orders for the side */
    StandingOrder *last_order;
    char *uorders;		/* bit vector of types that have orders */
    /* View layers. */
    char *terrview;		/* ptr to view of terrain */
    char **auxterrview;		/* ptr to view of aux terrain */
    short *terrviewdate;	/* ptr to dates of view of terrain */
    short **auxterrviewdate;	/* ptr to dates of view of aux terrain */
    short *unitview;		/* ptr to view of units */          
    short *unitviewdate;	/* ptr to dates of view of units */          
    short **materialview;	/* ptr to view of cell materials */
    short **materialviewdate;	/* ptr to dates of view of cell materials */
    short *tempview;		/* ptr to view of temperature */
    short *tempviewdate;	/* ptr to dates of view of temperature */
    short *cloudview;		/* ptr to view of clouds */
    short *cloudbottomview;	/* ptr to view of cloud bottoms */
    short *cloudheightview;	/* ptr to view of cloud heights */
    short *cloudviewdate;	/* ptr to dates of view of clouds */
    short *windview;		/* ptr to view of winds */
    short *windviewdate;	/* ptr to dates of view of winds */
    /* Progress and status within game. */
    short ingame;		/* true if side participating in game */
    short everingame;		/* true if side ever participated in a turn */
    short priority;		/* overall action priority of this side */
    short status;		/* -1/0/1 for lost/draw/won */
    short *scores;		/* an array of scores managed by scorekeepers */
    Obj *rawscores;		/* score data as read from file */
    short willingtodraw;	/* will this side go along with quitters? */
    short autofinish;		/* turn done when all units acted */
    short finishedturn;		/* true when side wants to go to next turn */
    short turntimeused;		/* seconds used this turn */
    short totaltimeused;	/* total seconds used */
    short timeouts;		/* total timeouts used */
    short timeoutsused;		/* total timeouts used */
    /* Setup. */
    short advantage;		/* actual advantage */
    short minadvantage;		/* max advantage requestable during init */
    short maxadvantage;		/* min of same */
    Obj *instructions;		/* notes to player about the game */
    /* Other. */
    short last_notice_date;	/* last turn during which notice was posted */
    short realtimeout;		/* how long to wait before just going ahead */
    long startbeeptime;		/* after this time, beep to signal next turn */
    short *gaincounts;
    short *losscounts;
    long **atkstats;
    long **hitstats;
    /* Pointers to the other major structures of a side. */
    struct a_player *player;	/* pointer to data about the player */
    short playerid;		/* numeric id of the player */
    struct a_ui *ui;		/* pointer to all the user interface data */
    Obj *uidata;		/* readable/writable form of user interface state */
    short aitype;		/* type of AI making decisions */		
    struct a_ai *ai;		/* pointer to the AI making decisions */
    Obj *aidata;		/* readable/writable form of AI state */
    /* Misc working slots, never saved. */
    short startx, starty;	/* approx center of side's "country" */
    short busy;			/* true when side state not saveable */
    short finalradius;
    short willingtosave;	/* will this side go along with saving the game? */
    short see_all;		/* true if this side sees everything */
    short may_set_see_all;	/* true if player can ask to see everything */
#ifdef DESIGNERS
    short designer;		/* true if side is doing scenario design */
#endif /* DESIGNERS */
    /* Various caches, should be possible to recalc as needed. */
    short *uavail;		/* vector of which types allowed on this side */
    struct a_unit *unithead;	/* points to list of all units on this side */
    struct a_unitvector *actionvector;  /* vector of acting units */
    short numwaiting;		/* number of units waiting to get orders */
    short *coverage;		/* indicates how many looking at this cell */
    short *alt_coverage;	/* indicates minimum altitude visible */
    short *numunits;		/* number of units the side has */
    short *numlive;		/* number of live & in-play units the side has */
    short numacting;		/* number of units that can do things */
    short numfinished;		/* number of units that have finished acting */
    long turnstarttime;		/* real time at start of turn */
    long lasttime;		/* when clock started counting down again */
    long turnfinishtime;	/* real time when we've finished interacting */
    long laststarttime;		/* ? */
    int point_value_cache;
    int point_value_valid;
    /* Links. */
    struct a_side *next;	/* pointer to next in list */
} Side;

/* Some convenient macros. */

#define side_in_play(side) (side == NULL || side->ingame)

/* Iteration over all sides. */

/* The first "side" is just the independent units, don't usually look at it. */

#define for_all_sides(v) for (v = sidelist->next; v != NULL; v = v->next)

/* But sometimes we need to treat independents if they were on a side. */

#define for_all_sides_plus_indep(v)  \
  for (v = sidelist; v != NULL; v = v->next)

/* Macros for accessing and setting a side's view data.  In general, interfaces
   should go through these macros instead of accessing the world's state
   directly. */

#define terrain_view(s,x,y)  \
  ((s)->terrview ? aref((s)->terrview, x, y) : buildtview(terrain_at(x, y)))

#define set_terrain_view(s,x,y,v)  \
  ((s)->terrview ? aset((s)->terrview, x, y, v) : 0)

#define buildtview(t) ((t) + 1)

#define vterrain(v) ((v) - 1)

#define UNSEEN (0)

#define terrain_view_date(s,x,y)  \
  ((s)->terrviewdate ? aref((s)->terrviewdate, x, y) : (-1))

#define set_terrain_view_date(s,x,y,v)  \
  ((s)->terrviewdate ? aset((s)->terrviewdate, x, y, v) : 0)

#define aux_terrain_view(s,x,y,t)  \
  (((s)->auxterrview && (s)->auxterrview[t]) ? aref((s)->auxterrview[t], x, y) : (aux_terrain_defined(t) ? aux_terrain_at(x, y, t) : 0))

#define set_aux_terrain_view(s,x,y,t,v)  \
  (((s)->auxterrview && (s)->auxterrview[t]) ? aset((s)->auxterrview[t], x, y, v) : 0)

#define aux_terrain_view_date(s,x,y,t)  \
  (((s)->auxterrviewdate && (s)->auxterrviewdate[t]) ? aref((s)->auxterrviewdate[t], x, y) : (-1))

#define set_aux_terrain_view_date(s,x,y,t,v)  \
  (((s)->auxterrviewdate && (s)->auxterrviewdate[t]) ? aset((s)->auxterrviewdate[t], x, y, v) : 0)

/* Basic view is encoded as hhhhsssssuuuuuuu + 1, where the u's are the unit
   type, s's are the side number, and h's are approx hp/size.  0 == empty. */
   
#define unit_view(s,x,y)  \
  ((s)->unitview ? aref((s)->unitview, x, y) : EMPTY)

#define set_unit_view(s,x,y,v)  \
  ((s)->unitview ? aset((s)->unitview, x, y, v) : 0)

#define builduview(s,u) (((((s) & 0x1f) << 7) | ((u) & 0x7f)) + 1)

#define vside(v) ((((v) - 1) >> 7) & 0x1f)

#define vtype(v) (((v) - 1) & 0x7f)

#define builduviewhp(s,u,hp)  \
  (((((hp) & 0x0f) << 12) | (((s) & 0x1f) << 7) | ((u) & 0x7f)) + 1)

#define vhp(v) ((((v) - 1) >> 12) & 0x0f)

#define EMPTY (0)

#define appears_empty(v) ((v) == EMPTY)

#define vside_indep(v) (vside(v) == 0)

#define unit_view_date(s,x,y)  \
  ((s)->unitviewdate ? aref((s)->unitviewdate, x, y) : (-1))

#define set_unit_view_date(s,x,y,v)  \
  ((s)->unitviewdate ? aset((s)->unitviewdate, x, y, v) : 0)

/* Manipulation of view of cell materials. */

#define material_view(s,x,y,m)  \
  (((s)->materialview && (s)->materialview[m]) ? aref((s)->materialview[m], x, y) : (cell_material_defined(m) ? material_at(x, y, m) : 0))

#define set_material_view(s,x,y,m,v)  \
  (((s)->materialview && (s)->materialview[m]) ? aset((s)->materialview[m], x, y, v) : 0)

#define material_view_date(s,x,y,m)  \
  (((s)->materialviewdate && (s)->materialviewdate[m]) ? aref((s)->materialviewdate[m], x, y) : (-1))

#define set_material_view_date(s,x,y,m,v)  \
  (((s)->materialviewdate && (s)->materialviewdate[m]) ? aset((s)->materialviewdate[m], x, y, v) : 0)

/* Manipulation of view of weather. */

#define temperature_view(s,x,y)  \
  ((s)->tempview ? aref((s)->tempview, x, y) : (temperatures_defined() ? temperature_at(x, y) : 0))

#define set_temperature_view(s,x,y,v)  \
  ((s)->tempview ? aset((s)->tempview, x, y, v) : 0)

#define temperature_view_date(s,x,y)  \
  ((s)->tempviewdate ? aref((s)->tempviewdate, x, y) : (-1))

#define set_temperature_view_date(s,x,y,v)  \
  ((s)->tempviewdate ? aset((s)->tempviewdate, x, y, v) : 0)

#define cloud_view(s,x,y)  \
  ((s)->cloudview ? aref((s)->cloudview, x, y) : (clouds_defined() ? raw_cloud_at(x, y) : 0))

#define set_cloud_view(s,x,y,v)  \
  ((s)->cloudview ? aset((s)->cloudview, x, y, v) : 0)

#define cloud_view_date(s,x,y)  \
  ((s)->cloudviewdate ? aref((s)->cloudviewdate, x, y) : (-1))

#define set_cloud_view_date(s,x,y,v)  \
  ((s)->cloudviewdate ? aset((s)->cloudviewdate, x, y, v) : 0)

#define cloud_bottom_view(s,x,y)  \
  ((s)->cloudbottomview ? aref((s)->cloudbottomview, x, y) : (cloud_bottoms_defined() ? raw_cloud_bottom_at(x, y) : 0))

#define set_cloud_bottom_view(s,x,y,v)  \
  ((s)->cloudbottomview ? aset((s)->cloudbottomview, x, y, v) : 0)

#define cloud_bottom_view_date(s,x,y)  \
  ((s)->cloudbottomviewdate ? aref((s)->cloudbottomviewdate, x, y) : (-1))

#define set_cloud_bottom_view_date(s,x,y,v)  \
  ((s)->cloudbottomviewdate ? aset((s)->cloudbottomviewdate, x, y, v) : 0)

#define cloud_height_view(s,x,y)  \
  ((s)->cloudheightview ? aref((s)->cloudheightview, x, y) : (cloud_heights_defined() ? raw_cloud_height_at(x, y) : 0))

#define set_cloud_height_view(s,x,y,v)  \
  ((s)->cloudheightview ? aset((s)->cloudheightview, x, y, v) : 0)

#define cloud_height_view_date(s,x,y)  \
  ((s)->cloudheightviewdate ? aref((s)->cloudheightviewdate, x, y) : (-1))

#define set_cloud_height_view_date(s,x,y,v)  \
  ((s)->cloudheightviewdate ? aset((s)->cloudheightviewdate, x, y, v) : 0)

#define wind_view(s,x,y)  \
  ((s)->windview ? aref((s)->windview, x, y) : (winds_defined() ? raw_wind_at(x, y) : CALM))

#define set_wind_view(s,x,y,v)  \
  ((s)->windview ? aset((s)->windview, x, y, v) : 0)

#define wind_view_date(s,x,y)  \
  ((s)->windviewdate ? aref((s)->windviewdate, x, y) : (-1))

#define set_wind_view_date(s,x,y,v)  \
  ((s)->windviewdate ? aset((s)->windviewdate, x, y, v) : 0)

/* Basic manipulation of vision coverage cache layer. */

#define cover(s,x,y)  \
  ((s)->coverage ? aref((s)->coverage, x, y) : 0)

#define set_cover(s,x,y,v)  \
  ((s)->coverage ? aset((s)->coverage, x, y, v) : 0)

#define add_cover(s,x,y,v)  \
  ((s)->coverage ? (aref((s)->coverage, x, y) += (v)) : 0)

#define alt_cover(s,x,y)  \
  ((s)->alt_coverage ? aref((s)->alt_coverage, x, y) : 0)

#define set_alt_cover(s,x,y,v)  \
  ((s)->alt_coverage ? aset((s)->alt_coverage, x, y, v) : 0)

/* Tests of who/what runs the side. */

#define side_wants_display(s) ((s)->player && (s)->player->displayname)

#define side_wants_ai(s) ((s)->player && (s)->player->aitypename)

#define side_has_display(s) (((s)->ui) != NULL)

#define side_has_ai(s) (((s)->ai) != NULL)

#define side_lost(s) ((s) != NULL && !(s)->ingame && (s)->status < 0)

#define side_drew(s) ((s) != NULL && !(s)->ingame && (s)->status == 0)

#define side_won(s) ((s) != NULL && !(s)->ingame && (s)->status > 0)

#define side_gain_count(s,u,r) (((s)->gaincounts)[num_gain_reasons*(u)+(r)])

#define side_loss_count(s,u,r) (((s)->losscounts)[num_loss_reasons*(u)+(r)])

#define side_atkstats(s,a,d) ((s)->atkstats[a] ? ((s)->atkstats[a])[d] : 0)

#define side_hitstats(s,a,d) ((s)->hitstats[a] ? ((s)->hitstats[a])[d] : 0)

#define terrain_visible(side, x, y)  \
  (all_see_all || (side)->see_all || (terrain_view((side), wrapx(x), (y)) != UNSEEN))

#define borders_visible(side, x, y, d)  \
  (all_see_all || (side)->see_all || seen_border((side), (x), (y), (d)))

#define units_visible(side, x, y)  \
  (all_see_all || (side)->see_all || (cover((side), wrapx(x), (y)) >= 1))

/* Side-related variables. */

extern Side *sidelist;
extern Side *lastside;
extern Side *indepside;
extern Side *curside;
extern Side *tmpside;

extern int numsides;
extern int numplayers;
#ifdef DESIGNERS
extern int numdesigners;
#endif

extern int all_see_all;
extern int all_see_all_terrain;

/* Definition of an agreement between sides. */

typedef struct a_agreement {
    short id;                 /* a unique id */
    char *typename;           /* a descriptive general name */
    char *name;               /* the specific name of this agreement */
    int state;                /* is this agreement in effect */
    SideMask drafters;        /* sides drafting the agreement */
    SideMask proposers;       /* sides ready to propose the draft agreement */
    SideMask signers;         /* proposed list of signers */
    SideMask willing;         /* sides that have indicated agreement so far */
    SideMask knownto;         /* sides that are aware of the signed agreement */
    struct a_obj *terms;      /* list of specific terms */
    short enforcement;        /* true if program should attempt to enforce terms */
    struct a_agreement *next;
} Agreement;

enum {
    draft,                    /* agreement is circulating among drafters */
    proposed,                 /* agreement is proposed to prospective signers */
    in_force,                 /* agreement is active */
    moribund                  /* agreement has expired */
};

/* Iteration over all agreements in the game. */

#define for_all_agreements(v) for (v = agreementlist; v != NULL; v = v->next)

#define any_agreements() (agreementlist != NULL)

#define side_signed_agreement(side,ag) ((ag)->signers[side_n(side)])

#define side_willing_agreement(side,ag) ((ag)->willing[side_n(side)])

#define side_knows_about_agreement(side,ag) ((ag)->knownto[side_n(side)])

/* Agreement-related variables. */

extern int numagreements;

extern Agreement *agreementlist;
extern Agreement *lastagreement;

/* Note: Can't use the "Unit" typedef below, must use "struct a_unit". */

extern void init_sides PARAMS ((void));
extern Side *create_side PARAMS ((void));
extern void init_side_unithead PARAMS ((Side *side));
extern int side_has_units PARAMS ((Side *side));
extern void init_doctrine PARAMS ((Side *side));
extern void init_self_unit PARAMS ((Side *side));
extern int init_view PARAMS ((Side *side));
extern void calc_start_xy PARAMS ((Side *side));
extern char *side_name PARAMS ((Side *side));
extern char *side_adjective PARAMS ((Side *side));
extern int side_number PARAMS ((Side *side));
extern Side *side_n PARAMS ((int n));
extern Side *find_side_by_name PARAMS ((char *str));
extern int side_controls_side PARAMS ((Side *side, Side *side2));
extern int side_controls_unit PARAMS ((Side *side, struct a_unit *unit));
extern int side_sees_unit PARAMS ((Side *side, struct a_unit *unit));
extern int side_sees_image PARAMS ((Side *side, struct a_unit *unit));
extern int num_units_in_play PARAMS ((Side *side, int u));
extern int num_units_incomplete PARAMS ((Side *side, int u));
extern struct a_unit *find_next_unit PARAMS ((Side *side, struct a_unit *prevunit));
extern struct a_unit *find_prev_unit PARAMS ((Side *side, struct a_unit *nextunit));
extern struct a_unit *find_next_actor PARAMS ((Side *side, struct a_unit *prevunit));
extern struct a_unit *find_prev_actor PARAMS ((Side *side, struct a_unit *nextunit));
extern struct a_unit *find_next_mover PARAMS ((Side *side, struct a_unit *prevunit));
extern struct a_unit *find_prev_mover PARAMS ((Side *side, struct a_unit *nextunit));
extern struct a_unit *find_next_awake_mover PARAMS ((Side *side, struct a_unit *prevunit));
extern struct a_unit *find_prev_awake_mover PARAMS ((Side *side, struct a_unit *nextunit));
extern int side_initacp PARAMS ((Side *side));
extern int side_acp PARAMS ((Side *side));
extern int side_acp_reserved PARAMS ((Side *side));
extern int using_tech_levels PARAMS ((void));
extern void remove_side_from_game PARAMS ((Side *side));
extern int num_displayed_sides PARAMS ((void));
extern void set_side_name PARAMS ((Side *side, Side *side2, char *newname));
extern void become_designer PARAMS ((Side *side));
extern void become_nondesigner PARAMS ((Side *side));
extern int trusted_side PARAMS ((Side *side1, Side *side2));
extern void set_trust PARAMS ((Side *side, Side *side2, int val));
extern void set_mutual_trust PARAMS ((Side *side, Side *side2, int val));
extern void set_autofinish PARAMS ((Side *side, int value));
extern int enemy_side PARAMS ((Side *s1, Side *s2));
extern int allied_side PARAMS ((Side *s1, Side *s2));
extern int neutral_side PARAMS ((Side *s1, Side *s2));
extern void set_willing_to_save PARAMS ((Side *side, int flag));
extern void set_willing_to_draw PARAMS ((Side *side, int flag));
extern void reveal_side PARAMS ((Side *sender, Side *recipient, int *types));
extern void send_message PARAMS ((Side *side, SideMask sidemask, char *str));
extern void receive_message PARAMS ((Side *side, Side *sender, char *str));
extern void calc_vision PARAMS ((Side *side));
extern void all_see_occupy PARAMS ((struct a_unit *unit, int x, int y, int inopen));
extern void all_see_leave PARAMS ((struct a_unit *unit, int x, int y, int inopen));
extern void cover_area PARAMS ((Side *side, struct a_unit *unit, int x0, int y0, int x1, int y1));
extern void reset_coverage PARAMS ((void));
extern void calc_coverage PARAMS ((Side *side));
extern void reset_all_views PARAMS ((void));
extern void reset_view PARAMS ((Side *side));
extern void react_to_seen_unit PARAMS ((Side *side, struct a_unit *unit, int x, int y));
extern void all_see_cell PARAMS ((int x, int y));
extern int see_cell PARAMS ((Side *side, int x, int y));
extern void see_exact PARAMS ((Side *side, int x, int y));
extern int seen_border PARAMS ((Side *side, int x, int y, int dir));
extern char *side_desig PARAMS ((Side *side));

extern Side *parse_side_spec PARAMS ((char *str));

extern Doctrine *new_doctrine PARAMS ((int id));
extern Doctrine *find_doctrine PARAMS ((int id));
extern Doctrine *find_doctrine_by_name PARAMS ((char *name));
extern Doctrine *clone_doctrine PARAMS ((Doctrine *doctrine));

extern StandingOrder *new_standing_order PARAMS ((void));
extern void add_standing_order PARAMS ((Side *side, StandingOrder *sorder, int pos));
extern int parse_standing_order PARAMS ((Side *side, char *cmdstr));
extern char *parse_unit_types PARAMS ((Side *side, char *str, char *utypevec));
extern char *parse_order_cond PARAMS ((Side *side, char *str, StandingOrder *sorder));
extern char *get_next_arg PARAMS ((char *str, char *buf, char **rsltp));

extern void init_agreements PARAMS ((void));
extern Agreement *create_agreement PARAMS ((int id));
extern char *agreement_desig PARAMS ((Agreement *ag));

extern int load_side_config PARAMS ((Side *side));

extern int set_side_self_unit PARAMS ((Side *side, struct a_unit *unit));

#ifdef DESIGNERS
extern void paint_view PARAMS ((Side *side, int x, int y, int r, int tview, int uview));
#endif /* DESIGNERS */
