/* Definitions common to all AIs.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

typedef enum {
    nobrains = 0,
    mplayertype = 1
    /* plus other types as defined */
#ifdef MPW_C
    /* Works around an MPW C bug where byte-sized enums lose - symptom
       is that AIs never do anything. */
    , aitype_intifier = 1000000
#endif /* MPW_C */
} AIType;

typedef struct a_ai_op {
    char *name;
    int (*to_test_compat) PARAMS ((void));
    void (*to_init) PARAMS ((Side *side));
    void (*to_init_turn) PARAMS ((Side *side));
    void (*to_decide_plan) PARAMS ((Side *side, Unit *unit));
    void (*to_react_to_unit_loss) PARAMS ((Side *side, Unit *unit));
    void (*to_react_to_action_result) PARAMS ((Side *side, Unit *unit,
					      int rslt));
    void (*to_react_to_task_result) PARAMS ((Side *side, Unit *unit,
					    Task *task, TaskOutcome rslt));
	void (*to_react_to_new_side) PARAMS ((Side *side, Side *side2));
	int (*planning_to_capture) PARAMS ((Side *side, int u, int x, int y));
    int (*to_guide_explorer) PARAMS ((Side *side, Unit *unit));
    int (*preferred_build_type) PARAMS ((Side *side, Unit *unit, int plantype));
    void (*to_analyze_after_moves) PARAMS ((Side *side, int numacted));
    void (*to_finish_movement) PARAMS ((Side *side));
    void (*to_receive_message) PARAMS ((Side *side, Side *sender, char *str));
    Obj *(*to_save_state) PARAMS ((Side *side));
    int (*region_at) PARAMS ((Side *side, int x, int y));
    char *(*at_desig) PARAMS ((Side *side, int x, int y));
    int dummy;
} AI_ops;

/* Definition common to all ai types. (?) */

typedef struct a_ai {
  int dummy;
} AI;

#define side_ai_type(s) ((s)->aitype)

/* Add declaration of AI hooks here. */

extern void init_ai_types PARAMS ((void));
extern void init_ai PARAMS ((Side *side));
extern void set_side_ai PARAMS ((Side *side, char *typename));
extern void ai_init_turn PARAMS ((Side *side));
extern void ai_decide_plan PARAMS ((Side *side, Unit *unit));
extern void ai_react_to_unit_loss PARAMS ((Side *side, Unit *unit));
extern void ai_react_to_action_result PARAMS ((Side *side, Unit *unit,
					      int rslt));
extern void ai_react_to_task_result PARAMS ((Side *side, Unit *unit,
					    Task *task, TaskOutcome rslt));
extern void ai_react_to_new_side PARAMS ((Side *side, Side *side2));
extern int ai_planning_to_capture PARAMS ((Side *side, int u, int x, int y));
extern int ai_guide_explorer PARAMS ((Side *side, Unit *unit));
extern int ai_preferred_build_type PARAMS ((Side *side, Unit *unit,
					   int plantype));
extern void ai_analyze_after_moves PARAMS ((Side *side, int numacted));
extern void ai_finish_movement PARAMS ((Side *side));
extern void ai_receive_message PARAMS ((Side *side, Side *sender, char *str));
extern void ai_save_state PARAMS ((Side *side));
extern int ai_region_at PARAMS ((Side *side, int x, int y));
extern char *ai_at_desig PARAMS ((Side *side, int x, int y));

extern void ai_init_shared PARAMS ((void));
extern int basic_worth PARAMS ((int u));
extern int basic_hit_worth PARAMS ((int u, int e));
extern int basic_fire_worth PARAMS ((int u, int e));
extern int basic_capture_worth PARAMS ((int u, int e));
extern int basic_transport_worth PARAMS ((int u, int e));
extern int unit_strength PARAMS ((int u));
extern void display_assessment PARAMS ((void));

extern int is_base_for PARAMS ((int u1, int u2));
extern int is_carrier_for PARAMS ((int u1, int u2));

extern void set_u_is_base PARAMS ((int u, int n));
extern void set_u_is_transport PARAMS ((int u, int n));
extern void set_u_is_carrier PARAMS ((int u, int n));
extern void set_u_is_base_builder PARAMS ((int u, int n));
extern void set_u_can_make PARAMS ((int u, int n));
extern void set_u_can_capture PARAMS ((int u, int n));
extern void set_u_bw PARAMS ((int u, int n));
extern void set_uu_bhw PARAMS ((int u1, int u2, int v));
extern void set_uu_bfw PARAMS ((int u1, int u2, int v));
extern void set_uu_bcw PARAMS ((int u1, int u2, int v));
extern void set_uu_btw PARAMS ((int u1, int u2, int v));
