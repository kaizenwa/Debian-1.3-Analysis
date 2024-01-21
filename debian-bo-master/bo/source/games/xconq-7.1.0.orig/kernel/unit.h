/* Definitions relating to units in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* The unit structure should be small, because there may be many of them.
   Unit semantics go in this structure, while unit brains go into the
   act/plan.  Test: a unit that is like a rock and can't do anything at all
   just needs basic slots, plan needn't be allocated.  Another test:
   unit should still function correctly after its current plan has been
   destroyed and replaced with another. */

typedef struct a_unit {
    short type;			/* type */
    short id;		  	/* truly unique id number */
    char *name;			/* the name, if given */
    int number;			/* semi-unique number */
    short x, y, z;	     	/* position of unit in world */
    struct a_side *side;	/* whose side this unit is on */
    struct a_side *origside;	/* the first side this unit was on */
    short hp;			/* how much more damage each part can take */
    short hp2;		 	/* buffer for next value of hp */
    short cp;		  	/* state of construction */
    short cxp;		 	/* combat experience */
    short morale;		/* morale */
    struct a_unit *transport;	/* pointer to transporting unit if any */
    SideMask spotted;		/* which sides always see us (bit vector) */
    short *supply;		/* how much supply we're carrying */
    short *tooling;	    	/* level of preparation for construction */
    short *opinions;		/* opinion of each side, own side and others */
    struct a_actorstate *act;	/* the unit's current actor state */
    struct a_plan *plan;	/* the unit's current plan */
    struct a_unit_extras *extras;  /* pointer to optional stuff */
    char *aihook;		/* used by AI to keep info about this unit */
    char *uihook;		/* used by interfaces for their own purposes */
    /* Following slots are never saved. */
    struct a_unit *occupant;	/* pointer to first unit being carried */
    struct a_unit *nexthere;	/* pointer to fellow occupant */
    struct a_unit *prev;	/* previous unit in list of side's units */
    struct a_unit *next;	/* next unit in list of side's units */
    struct a_unit *unext;	/* next unit in list of all units */
    short prevx, prevy;		/* where were we last */
    short flags;		/* assorted flags */
} Unit;

/* The unit extras structure stores properties that are (usually)
   uncommon or special-purpose.  It is allocated as needed, access
   is through macros, and this does not appear as a separate
   structure when saving/restoring.  If a value is likely to be
   filled in for most or all units in several different game designs,
   then it should be in the main structure (even at the cost of extra
   space), rather than here. */

typedef struct a_unit_extras {
    short point_value;		/* individual point value for this unit */
    short appear;		/* turn of appearance in game */
    short appearx, appeary;	/* variation around appearance location */
    short disappear;		/* turn of disappearance from game */
    Obj *hook;		 	/* placeholder for more optional stuff */
} UnitExtras;

/* Some convenient macros. */

/* Since it is possible for a unit to change sides and therefore
   prev/next pointers while iterating using the macros below, one
   must be very careful either that unit sides can't change during
   the loop, or else to maintain a temp var that can be used to
   repair the iteration.  This also applies to units dying. */

/* Iteration over all units. */
/* Careful with this one, can run afoul of precedence rules since no
   brace to enclose the outer loop. */

#define for_all_units(v)  \
    for (v = unitlist; v != NULL; v = v->unext)

/* Iteration over all units on a given side. */

#define for_all_side_units(s,v) \
    for (v = (s)->unithead->next; v != (s)->unithead; v = v->next)

/* Iteration over all occupants of a unit (but not sub-occupants). */

#define for_all_occupants(u1,v) \
  for (v = (u1)->occupant; v != NULL; v = v->nexthere)

#define is_unit(unit) ((unit) != NULL && is_unit_type((unit)->type))

#define alive(unit) ((unit)->hp > 0)

#define indep(unit) ((unit)->side == NULL || (unit)->side == indepside)

#define completed(unit) \
  ((unit)->cp >= completenesses[(unit)->type])
/*  ((unit)->cp >= (u_cp((unit)->type) / u_parts((unit)->type))) */

#define fullsized(unit) \
  ((unit)->cp >= u_cp((unit)->type))

/* Extractor for the actual altitude of an airborne unit. */

#define unit_alt(unit) ((unit)->z & 1 == 0 ? ((unit)->z >> 1) : 0)

/* Extractor for the connection a unit is on. */

#define unit_conn(unit) ((unit)->z & 1 == 1 ? ((unit)->z >> 1) : NONTTYPE)

/* This is true if the unit is on the board somewhere. */

#define is_present(unit) in_play(unit)

#define in_play(unit) \
  (is_unit(unit) && alive(unit) && inside_area((unit)->x, (unit)->y))

#define in_action(unit) is_active(unit)

#define is_acting(unit) is_active(unit)

#define is_active(unit) (in_play(unit) && completed(unit))

/* (could use bit fields in struct I suppose...) */

#define DETONATE_FLAG_BIT 0

#define was_detonated(unit) \
  ((unit)->flags & (1 << DETONATE_FLAG_BIT))

#define set_was_detonated(unit,val) \
  ((unit)->flags = \
    (((unit)->flags & ~(1 << DETONATE_FLAG_BIT)) \
     | (((val) ? 1 : 0) << DETONATE_FLAG_BIT)))

/* Extractions for the two parts of an attitude/feeling. */

#define intensity(att) (((att) >> 8) & 0xff)

#define bias(att) (((att) & 0xff) - 128)

#define unit_point_value(unit) ((unit)->extras ? (unit)->extras->point_value : -1)

#define unit_appear_turn(unit) ((unit)->extras ? (unit)->extras->appear : -1)

#define unit_appear_x(unit) ((unit)->extras ? (unit)->extras->appearx : -1)

#define unit_appear_y(unit) ((unit)->extras ? (unit)->extras->appeary : -1)

#define unit_disappear_turn(unit) ((unit)->extras ? (unit)->extras->disappear : -1)

/* We test the hook for NULL, because lispnil is not NULL and we can't
   count on the hook being initialized when extras are allocated. */

#define unit_hook(unit)  \
  (((unit)->extras && (unit)->extras->hook) ? (unit)->extras->hook : lispnil)

/* A sortable vector of units, generally useful. */

/* The kinds of sort keys available for list windows. */

enum sortkeys {
    bynothing,
    bytype,
    byname,
    byactorder,
    bylocation,
    byside,
    numsortkeytypes
};

/* Can sort on as many as five keys. */

#define MAXSORTKEYS 5

typedef struct a_unitvectorentry {
    Unit *unit;
    int flag;
} UnitVectorEntry;

typedef struct a_unitvector {
    int size;
    int numunits;
    enum sortkeys sortkeys[MAXSORTKEYS];
    UnitVectorEntry units[1];
} UnitVector;

/* Types of primitive unit actions. */

typedef enum actiontype {

#undef  DEF_ACTION
#define DEF_ACTION(name,CODE,args,prepfn,dofn,checkfn,argdecl,doc) CODE,

#include "action.def"

    NUMACTIONTYPES

} ActionType;

typedef struct a_actiondefn {
    ActionType typecode;
    char *name;
    char *argtypes;
#ifdef THINK_C
    int (*dofn) PARAMS ((Unit *unit, Unit *unit2, ...));
    int (*checkfn) PARAMS ((Unit *unit, Unit *unit2, ...));
#else
    int (*dofn) PARAMS (());
    int (*checkfn) PARAMS (());
#endif
} ActionDefn;

#define MAXACTIONARGS 4

typedef struct a_action {
    ActionType type;		/* the type of the action */
    short args[MAXACTIONARGS];	/* assorted parameters */
    short actee;		/* the unit being affected by action */
    struct a_action *next;	/* chain to next action */
} Action;

typedef struct a_actorstate {
    short initacp;		/* how much we can still do */
    short acp;			/* how much we can still do */
    short actualactions;	/* actions actually done this turn */
    short actualmoves;		/* cells actually covered this turn */
    Action nextaction;
} ActorState;

#define valid(x) ((x) == A_ANY_OK)

#define has_pending_action(unit)  \
  ((unit)->act && (unit)->act->nextaction.type != ACTION_NONE)

/* All the definitions that govern planning. */

/* A goal is a predicate object that can be tested to see whether it has
   been achieved.  As such, it is a relatively static object and may be
   shared. */

/* The different types of goals. */

typedef enum goaltype {

#undef  DEF_GOAL
#define DEF_GOAL(name,GOALTYPE,args) GOALTYPE,

#include "goal.def"

    g_t_dummy
} GoalType;

typedef struct a_goaldefn {
    char *name;
    char *argtypes;
} GoalDefn;

/* The goal structure proper. */

#define MAXGOALARGS 5

typedef struct a_goal {
    GoalType type;
    short tf;
    Side *side;
    short args[MAXGOALARGS];
} Goal;

extern Goal *create_goal PARAMS ((GoalType type, Side *side, int tf));
extern int cell_unknown PARAMS ((int x, int y));
extern int enemies_present PARAMS ((int x, int y));
extern int goal_truth PARAMS ((Side *side, Goal *goal));
extern char *goal_desig PARAMS ((Goal *goal));

/* A task is a single executable element of a unit's plan.  Each task type
   is something that has been found useful or convenient to encapsulate as
   a step in a plan. */

typedef enum a_tasktype {

#undef  DEF_TASK
#define DEF_TASK(name,CODE,argtypes,fn) CODE,

#include "task.def"

    NUMTASKTYPES
} TaskType;

typedef enum a_taskoutcome {
  TASK_UNKNOWN,
  TASK_FAILED,
  TASK_IS_INCOMPLETE,
  TASK_PREPPED_ACTION,
  TASK_IS_COMPLETE
} TaskOutcome;

#define MAXTASKARGS 6

typedef struct a_task {
    TaskType type;		/* the kind of task we want to do */
    short args[MAXTASKARGS];	/* arguments */
    short execnum;		/* how many times this has been done */
    short retrynum;		/* number of immed failures so far */
    struct a_task *next;	/* the next task to undertake */
} Task;

typedef struct a_taskdefn {
    char *name;
    char *argtypes;
    TaskOutcome (*exec) PARAMS ((Unit *unit, Task *task));
} TaskDefn;

/* A plan is what a single unit uses to make decisions, both for itself and
   for any other units it commands.  Any unit that can act at all has a
   plan object.  A plan collects lots of unit behavior, but its most
   important structure is the task queue, which contains a list of what
   to do next, in order. */

/* Plan types distinguish several kinds of usages. */

typedef enum plantype {

#undef  DEF_PLAN
#define DEF_PLAN(name,CODE) CODE,

#include "plan.def"

    NUMPLANTYPES
} PlanType;

typedef struct a_plan {
    PlanType type;		/* general type of plan that we've got here */
    short creation_turn;	/* turn at which this plan was created */
    short initial_turn;		/* turn at which this plan is to be done */
    short final_turn;		/* turn to deactivate this plan */
    short asleep;		/* true if the unit is doing nothing */
    short reserve;		/* true if unit waiting until next turn */
    short delayed;
    short waitingfortasks;	/* true if waiting to be given a task */
    short aicontrol;		/* true if an AI can operate on the unit */
    short supply_alarm;
    short supply_is_low;
    short waitingfortransport;
    struct a_goal *maingoal;	/* the main goal of this plan */
    struct a_goal *formation;	/* goal to keep in a formation */
    struct a_task *tasks;	/* pointer to chain of sequential tasks */
    /* Not saved/restored. (little value, some trouble to do) */
    struct a_unit *funit;	/* pointer to unit keeping formation */
    Action lastaction;	 	/* a copy of the last action attempted */
    short lastresult;		/* that action's outcome */
    short execs_this_turn;
} Plan;

#define for_all_tasks(plan,task)  \
  for (task = (plan)->tasks; task != NULL; task = task->next)

/* Global unit variables. */

extern Unit *unitlist;
extern Unit *tmpunit;

extern int numunits;
extern int numliveunits;

extern short *completenesses;

extern enum sortkeys tmpsortkeys[];

extern ActionDefn actiondefns[];

extern GoalDefn goaldefns[];

extern TaskDefn taskdefns[];

extern char *plantypenames[];

/* Declarations of unit-related functions. */

extern void init_units PARAMS ((void));
extern Unit *create_bare_unit PARAMS ((int type));
extern Unit *create_unit PARAMS ((int type, int makebrains));
extern void init_unit_actorstate PARAMS ((Unit *unit, int flagacp));
extern void init_unit_plan PARAMS ((Unit *unit));
extern void init_unit_tooling PARAMS ((Unit *unit));
extern void init_unit_opinions PARAMS ((Unit *unit, int nsides));
extern void init_unit_extras PARAMS ((Unit *unit));
extern void change_unit_type PARAMS ((Unit *unit, int newtype, int reason));
extern int max_builds PARAMS ((int u));
extern int enter_cell PARAMS ((Unit *unit, int x, int y));
extern int can_occupy_cell PARAMS ((Unit *unit, int x, int y));
extern int type_can_occupy_cell PARAMS ((int u, int x, int y));
extern int can_occupy_cell_without PARAMS ((Unit *unit, int x, int y, Unit *unit3));
extern int type_can_occupy_cell_without PARAMS ((int u, int x, int y, Unit *unit3));
extern void enter_cell_aux PARAMS ((Unit *unit, int x, int y));
extern int can_occupy PARAMS ((Unit *unit, Unit *transport));
extern int can_carry PARAMS ((Unit *transport, Unit *unit));
extern int type_can_occupy PARAMS ((int u, Unit *transport));
extern int can_occupy_type PARAMS ((Unit *unit, int u2));
extern int can_carry_type PARAMS ((Unit *transport, int u));
extern void enter_transport PARAMS ((Unit *unit, Unit *transport));
extern void leave_cell PARAMS ((Unit *unit));
extern void leave_cell_aux PARAMS ((Unit *unit));
extern void leave_transport PARAMS ((Unit *unit));
extern void eject_excess_occupants PARAMS ((Unit *unit));
extern void eject_occupant PARAMS ((Unit *unit, Unit *occ));
extern void change_unit_side PARAMS ((Unit *unit, Side *newside, int reason, Unit *agent));
extern int unit_allowed_on_side PARAMS ((Unit *unit, Side *side));
extern int test_class_membership PARAMS ((Obj *leaf));
extern int type_allowed_on_side PARAMS ((int u, Side *side));
extern int unit_trusts_unit PARAMS ((Unit *unit1, Unit *unit2));
extern int set_unit_side PARAMS ((Unit *unit, Side *side));
extern int set_unit_origside PARAMS ((Unit *unit, Side *side));
extern void set_unit_plan_type PARAMS ((Side *side, Unit *unit, int type));
extern void set_unit_asleep PARAMS ((Side *side, Unit *unit, int flag, int recurse));
extern void set_unit_reserve PARAMS ((Side *side, Unit *unit, int flag, int recurse));
extern void set_unit_ai_control PARAMS ((Side *side, Unit *unit, int flag, int recurse));
extern void set_unit_name PARAMS ((Side *side, Unit *unit, char *newname));
extern int disband_unit PARAMS ((Side *side, Unit *unit));
extern void kill_unit PARAMS ((Unit *unit, int reason));
extern void kill_unit_aux PARAMS ((Unit *unit, int reason));
extern void dispose_of_plan PARAMS ((Unit *unit));
extern void flush_dead_units PARAMS ((void));
extern void sort_units PARAMS ((void));
extern int moves_till_low_supplies PARAMS ((Unit *unit));
extern char *unit_desig PARAMS ((Unit *unit));
extern char *unit_desig_no_loc PARAMS ((Unit *unit));
extern char *utype_name_n PARAMS ((int u, int n));
extern char *shortest_unique_name PARAMS ((int u));
extern char *actorstate_desig PARAMS ((struct a_actorstate *as));
extern Unit *find_unit PARAMS ((int n));
extern Unit *find_unit_by_name PARAMS ((char *nm));
extern Unit *find_unit_by_number PARAMS ((int nb));
extern Unit *find_unit_dead_or_alive PARAMS ((int n));
extern int find_unit_name PARAMS ((char *str));
extern Unit *first_unit PARAMS ((Side *side));
extern void insert_unit PARAMS ((Unit *unithead, Unit *unit));
extern void delete_unit PARAMS ((Unit *unit));
extern int num_occupants PARAMS ((Unit *unit));
extern int num_units_at PARAMS ((int x, int y));
extern void check_all_units PARAMS ((void));
extern void check_unit PARAMS ((Unit *unit));
extern UnitVector *make_unit_vector PARAMS ((int initsize));
extern void clear_unit_vector PARAMS ((UnitVector *vec));
extern UnitVector *add_unit_to_vector PARAMS ((UnitVector *vec, Unit *unit, int flag));
extern void remove_unit_from_vector PARAMS ((UnitVector *vec, Unit *unit, int pos));
extern void sort_unit_vector PARAMS ((UnitVector *vec));
extern Obj *get_x_property PARAMS ((Unit *unit, int subkey));
extern Obj *get_x_property_by_name PARAMS ((Unit *unit, char *str));

extern int disband_unit_directly PARAMS ((Side *side, Unit *unit));

#ifdef DESIGNERS
extern Unit *designer_create_unit PARAMS ((Side *side, int u, int s, int x, int y));
extern int designer_teleport PARAMS ((Unit *unit, int x, int y, Unit *other));
extern int designer_change_side PARAMS ((Unit *unit, Side *side));
extern int designer_disband PARAMS ((Unit *unit));
#endif /* DESIGNERS */

/* Declarations of plan-related functions. */

extern void execute_plan PARAMS ((Unit *unit, int try));
extern int move_into_formation PARAMS ((Unit *unit));
extern void plan_offense PARAMS ((Unit *unit));
extern int do_for_occupants PARAMS ((Unit *unit));
extern void plan_offense_support PARAMS ((Unit *unit));
extern void set_construction PARAMS ((Unit *unit, int u, int num));
extern void plan_defense PARAMS ((Unit *unit));
extern void plan_exploration PARAMS ((Unit *unit));
extern void plan_explorer_support PARAMS ((Unit *unit));
extern int victim_here PARAMS ((int x, int y));
extern int worth_capturing PARAMS ((Side *side, int u2, Side *oside, int x, int y));
extern int go_after_victim PARAMS ((Unit *unit, int range));
extern int target_here PARAMS ((int x, int y));
extern int fire_at_opportunity PARAMS ((Unit *unit));
extern int resupply_if_low PARAMS ((Unit *unit));
extern int rearm_if_low PARAMS ((Unit *unit));
extern int supplies_here PARAMS ((Unit *unit, int x, int y, int m));
extern int indep_captureable_here PARAMS ((int x, int y));
extern int capture_indep_if_nearby PARAMS ((Unit *unit));
extern int useful_captureable_here PARAMS ((int x, int y));
extern int useful_type PARAMS ((Side *side, int u));
extern int capture_useful_if_nearby PARAMS ((Unit *unit));
extern int could_capture_any PARAMS ((int u));
extern void plan_random PARAMS ((Unit *unit));
extern void make_plausible_random_args PARAMS ((char *argtypestr, int i, short *args, Unit *unit));
extern void decide_plan PARAMS ((Side *side, Unit *unit));
extern int doctrine_allows_wait PARAMS ((Unit *unit));
extern void wait_for_orders PARAMS ((Unit *unit));
extern void decide_tasks PARAMS ((Unit *unit));
extern void random_walk PARAMS ((Unit *unit));
extern void reserve_unit PARAMS ((Side *side, Unit *unit));
extern void wake_unit PARAMS ((Unit *unit, int wakeocc, int reason, Unit *unit2));
extern void wake_area PARAMS ((Side *side, int x, int y, int n, int occs));
extern void set_formation PARAMS ((Unit *unit, Unit *leader, int x, int y, int dist, int flex));
extern void delay_unit PARAMS ((Unit *unit, int flag));
extern int find_base PARAMS ((Unit *unit, int (*pred)(void), int extra));
extern int maybe_return_home PARAMS ((Unit *unit));
extern int range_left PARAMS ((Unit *unit));
extern void plan_exploration_route PARAMS ((Unit *unit, int x, int y));
extern int route_to PARAMS ((Unit *unit, int x, int y));
extern int find_worths PARAMS ((int range));
extern int attack_worth PARAMS ((Unit *unit, int e));
extern int threat PARAMS ((Side *side, int u, int x0, int y0));
extern int should_build_base PARAMS ((Unit *unit));
extern int region_portion PARAMS ((int n, int u, int *units_close, int *adjterr));
extern void pop_task PARAMS ((Plan *plan));
extern int react_to_enemies PARAMS ((Unit *unit));
extern int move_patrol PARAMS ((Unit *unit));
extern int build_time PARAMS ((Unit *unit, int prod));
extern int clear_task_agenda PARAMS ((Plan *plan));
extern Plan *create_plan PARAMS ((void));
extern void free_plan PARAMS ((Plan *plan));
extern char *plan_desig PARAMS ((Plan *plan));
extern int might_be_captured PARAMS ((Unit *unit));
extern void force_global_replan PARAMS ((Side *side));
extern int units_nearby PARAMS ((int x, int y, int dist, int type));
extern int survive_to_build_base PARAMS ((Unit *unit));
extern int exact_survive_to_build_base PARAMS ((Unit *unit));
extern int base_here PARAMS ((int x, int y));
extern int any_base_here PARAMS ((int x, int y));
extern int neutral_base_here PARAMS ((int x, int y));
extern int base_nearby PARAMS ((Unit *unit, int range));
extern int any_base_nearby PARAMS ((Unit *unit, int range));
extern int neutral_base_nearby PARAMS ((Unit *unit, int range));
extern int occupant_could_capture PARAMS ((Unit *unit, int etype));
extern int can_capture_neighbor PARAMS ((Unit *unit));
extern int occupant_can_capture_neighbor PARAMS ((Unit *unit));
extern int find_closest_unit PARAMS ((Side *side, int x0, int y0, int maxdist, int (*pred)(void), int *rxp, int *ryp));
extern int fullness PARAMS ((Unit *unit));
extern int can_build PARAMS ((Unit *unit));
extern int can_move PARAMS ((Unit *unit));
extern int out_of_ammo PARAMS ((Unit *unit));
extern int good_haven_p PARAMS ((Side *side, int x, int y));
extern int haven_p PARAMS ((Unit *unit, int x, int y));
extern int shop_p PARAMS ((Unit *unit, int x, int y));
extern int survival_time PARAMS ((Unit *unit));
extern int regions_around PARAMS ((int u, int x, int y, int center));
extern Task *make_route_step PARAMS ((int x, int y, int priority));
extern Task *make_route_chain PARAMS ((int sx, int sy, int tx, int ty));
extern Task *find_route_aux PARAMS ((Unit *unit, int maxdist, int curlen, int fromdir, int sx, int sy, int fx, int fy, int tx, int ty, int flags));
extern int usable_cell PARAMS ((Unit *unit, int x, int y));
extern Task *find_route PARAMS ((Unit *unit, int maxdist, int tx, int ty));
extern Task *find_route_aux_nearest PARAMS ((Unit *unit, int maxdist, int curlen, int fromdir, int sx, int sy, int fx, int fy, int (*pathpred)(void), int (*destpred)(void), int flags));
extern Task *find_route_to_nearest PARAMS ((Unit *unit, int fx, int fy, int maxdist, int (*pathpred)(void), int (*destpred)(void)));
extern int explorable_cell PARAMS ((int x, int y));
extern int reachable_unknown PARAMS ((int x, int y));
extern int adj_known_ok_terrain PARAMS ((int x, int y, Side *side, int u));
extern int explore_reachable_cell PARAMS ((Unit *unit, int range));
extern int optimize_plan PARAMS ((Unit *unit));
extern int should_capture_maker PARAMS ((Unit *unit));
extern int no_possible_moves PARAMS ((Unit *unit));
extern int adj_known_passable PARAMS ((Side *side, int x, int y, int u));
extern int adj_obstacle PARAMS ((int type, int x, int y));
extern int normal_completion_time PARAMS ((int u, int u2));
extern int adj_unit PARAMS ((int x, int y));
extern int past_halfway_point PARAMS ((Unit *unit));

extern int self_build_base_for_self PARAMS ((Unit *unit));

extern int operating_range_worst PARAMS ((int u));
extern int operating_range_best PARAMS ((int u));

extern int terrain_always_impassable PARAMS ((int u, int t));

extern int execute_standing_order PARAMS ((Unit *unit, int addtask));
extern int can_build_or_help PARAMS ((Unit *unit));
extern int can_produce PARAMS ((Unit *unit));
extern int low_on_supplies_one PARAMS ((Unit *unit));
extern int low_on_ammo_one PARAMS ((Unit *unit));

/* Declarations of task-related functions. */

extern void init_tasks PARAMS ((void));
extern void allocate_task_block PARAMS ((void));
extern Task *create_task PARAMS ((TaskType type));
extern Task *clone_task PARAMS ((Task *oldtask));
extern int fire_can_damage PARAMS ((Unit *unit, Unit *unit2));
extern Unit *repair_here PARAMS ((int x, int y));
extern Unit *aux_resupply_here PARAMS ((Unit *unit));
extern Unit *resupply_here PARAMS ((int x, int y));
extern int can_auto_resupply_self PARAMS ((Unit *unit, int *materials, int numtypes));
extern TaskOutcome execute_task PARAMS ((Unit *unit));
extern TaskOutcome execute_task_aux PARAMS ((Unit *unit, Task *task));
extern int choose_move_dirs PARAMS ((Unit *unit, int tx, int ty, int shortest, int (*dirtest)(Unit *, int), void (*dirsort)(Unit *, int *, int), int *dirs));
extern int plausible_move_dir PARAMS ((Unit *unit, int dir));
extern void sort_directions PARAMS ((Unit *unit, int *dirs, int numdirs));
extern void free_task PARAMS ((Task *task));
extern void add_task PARAMS ((Unit *unit, int pos, Task *task));
extern Task *create_move_to_task PARAMS ((int x, int y));
extern void push_move_to_task PARAMS ((Unit *unit, int x, int y));
extern void set_move_to_task PARAMS ((Unit *unit, int x, int y));
extern Task *create_move_near_task PARAMS ((int x, int y, int dist));
extern void set_move_near_task PARAMS ((Unit *unit, int x, int y, int dist));
extern void push_move_near_task PARAMS ((Unit *unit, int x, int y, int dist));
extern Task *create_move_dir_task PARAMS ((int dir, int n));
extern void set_move_dir_task PARAMS ((Unit *unit, int dir, int n));
extern Task *create_build_task PARAMS ((int u2, int run));
extern void push_build_task PARAMS ((Unit *unit, int u2, int run));
extern Task *create_research_task PARAMS ((int u2, int n));
extern void push_research_task PARAMS ((Unit *unit, int u2, int n));
extern void set_hit_task PARAMS ((Unit *unit, int x, int y));
extern void push_specific_hit_task PARAMS ((Unit *unit, int x, int y, int u, int s));
extern void set_specific_hit_task PARAMS ((Unit *unit, int x, int y, int u, int s));
extern void push_hit_task PARAMS ((Unit *unit, int x, int y));
extern Task *create_capture_task PARAMS ((int x, int y));
extern void set_capture_task PARAMS ((Unit *unit, int x, int y));
extern void push_capture_task PARAMS ((Unit *unit, int x, int y));
extern void set_disband_task PARAMS ((Unit *unit));
extern Task *create_resupply_task PARAMS ((int m));
extern void set_resupply_task PARAMS ((Unit *unit, int m));
extern Task *create_occupy_task PARAMS ((Unit *transport));
extern void push_occupy_task PARAMS ((Unit *unit, Unit *transport));
extern Task *create_pickup_task PARAMS ((Unit *occ));
extern void push_pickup_task PARAMS ((Unit *unit, Unit *occ));
extern Task *create_produce_task PARAMS ((int m, int n));
extern void push_produce_task PARAMS ((Unit *unit, int m, int n));
extern Task *create_sentry_task PARAMS ((int n));
extern void set_sentry_task PARAMS ((Unit *unit, int n));
extern void push_sentry_task PARAMS ((Unit *unit, int n));
extern char *parse_task PARAMS ((Side *side, char *str, Task **taskp));
extern char *task_desig PARAMS ((Task *task));
extern Unit *find_unit_to_complete PARAMS ((Unit *unit, Task *task));

extern int repair_test PARAMS ((int x, int y));
extern int resupply_test PARAMS ((int x, int y));

#undef  DEF_ACTION
#define DEF_ACTION(name,code,args,PREPFN,dofn,CHECKFN,ARGDECL,doc)  \
  extern int PREPFN  PARAMS (ARGDECL);  \
  extern int CHECKFN PARAMS (ARGDECL);

#include "action.def"

