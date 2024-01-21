/* Main include file for Xconq.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This is the include file that nearly all Xconq source files
   should include (exceptions would be very low-level or generic
   sources). */

#include "config.h"
#include "misc.h"
#include "dir.h"
#include "lisp.h"
#include "module.h"
#include "game.h"
#include "player.h"
#include "side.h"
#include "unit.h"
#include "world.h"
#include "history.h"
#include "score.h"
#include "help.h"
#include "ai.h"
#include "system.h"

/* (should be in a movies.h?) */

enum movie_type {
  movie_null,
  movie_miss,
  movie_hit,
  movie_death,
  movie_nuke,
  movie_extra_0
};

typedef struct a_movie_type {
  enum movie_type type;
  char *name;
} MovieType;

/* Declarations of globally visible globals. */

extern int typesdefined;
extern int gamedefined;
extern int compromised;
extern int cursequence;
extern int beforestart;
extern int endofgame;
extern int gamestatesafe;
extern int interfacestatesafe;
extern int any_post_action_scores;
extern int any_people_side_changes;
extern int max_zoc_range;
extern int warnings_suppressed;
extern time_t game_start_in_real_time;
extern time_t turn_play_start_in_real_time;

extern short *u_possible;

/* Declarations of functions that must be supplied by an interface. */

extern void announce_read_progress PARAMS ((void));
extern void announce_lengthy_process PARAMS ((char *msg));
extern void announce_progress PARAMS ((int pctdone));
extern void finish_lengthy_process PARAMS ((void));

extern Player *add_default_player PARAMS ((void));

extern void init_ui PARAMS ((Side *side));

extern int active_display PARAMS ((Side *side));

extern void update_turn_display PARAMS ((Side *side, int rightnow));
extern void update_action_display PARAMS ((Side *side, int rightnow));
extern void update_action_result_display PARAMS ((Side *side, Unit *unit, int rslt, int rightnow));
extern void update_event_display PARAMS ((Side *side, HistEvent *hevt, int rightnow));
extern void update_fire_at_display PARAMS ((Side *side, Unit *unit, Unit *unit2, int m, int rightnow));
extern void update_fire_into_display PARAMS ((Side *side, Unit *unit, int x, int y, int z, int m, int rightnow));
extern void update_clock_display PARAMS ((Side *side, int rightnow));
extern void update_side_display PARAMS ((Side *side, Side *side2, int rightnow));
extern void update_unit_display PARAMS ((Side *side, Unit *unit, int rightnow));
extern void update_unit_acp_display PARAMS ((Side *side, Unit *unit, int rightnow));
extern void update_message_display PARAMS ((Side *side, Side *side2, char *str, int rightnow));
extern void update_cell_display PARAMS ((Side *side, int x, int y, int rightnow));
extern void update_all_progress_displays PARAMS ((char *str, int s));
extern void update_everything PARAMS ((void));
extern void flush_display_buffers PARAMS ((Side *side));

extern int schedule_movie PARAMS ((Side *side, enum movie_type movie, ...));
extern void play_movies PARAMS ((SideMask sidemask));

extern void ui_save_state PARAMS ((Side *side));

extern void action_point PARAMS ((Side *side, int x, int y));

extern void notify_all PARAMS ((char *str, ...));
extern void notify PARAMS ((Side *side, char *str, ...));
#ifdef ANSI_PROTOTYPES
extern void vnotify PARAMS ((Side *side, char *fmt, va_list ap));
#endif
extern void low_notify PARAMS ((Side *side, char *str));

extern void init_warning PARAMS ((char *str, ...));
extern void low_init_warning PARAMS ((char *str));
extern void init_error PARAMS ((char *str, ...));
extern void low_init_error PARAMS ((char *str));
extern void run_warning PARAMS ((char *str, ...));
extern void low_run_warning PARAMS ((char *str));
extern void run_error PARAMS ((char *str, ...));
extern void low_run_error PARAMS ((char *str));

extern void print_form PARAMS ((Obj *form));
extern void end_printing_forms PARAMS ((void));

extern void close_displays PARAMS ((void));

/* Declarations of functions not elsewhere declared. */

extern void init_library_path PARAMS ((char *path));
extern void init_data_structures PARAMS ((void));
extern void check_game_validity PARAMS ((void));
extern void calculate_globals PARAMS ((void));
extern void patch_object_references PARAMS ((void));
extern void make_trial_assignments PARAMS ((void));
extern Side *make_up_a_side PARAMS ((void));
extern void init_side_advantage PARAMS ((Side *side));
extern int add_side_and_player PARAMS ((void));
extern int exchange_players PARAMS ((int n, int n2));
extern int remove_side_and_player PARAMS ((void));
extern void run_synth_methods PARAMS ((void));
extern int make_random_date PARAMS ((int calls, int runs));
extern int make_weather PARAMS ((int calls, int runs));
extern void final_init PARAMS ((void));
extern void configure_sides PARAMS ((void));
extern void init_all_views PARAMS ((void));
extern void init_area_views PARAMS ((void));
extern int make_initial_materials PARAMS ((int calls, int runs));
extern void init_supply PARAMS ((Unit *unit));
extern int saved_game PARAMS ((void));
extern void init_side_balance PARAMS ((void));
extern void check_consistency PARAMS ((void));
extern void assign_players_to_sides PARAMS ((void));
extern char *version_string PARAMS ((void));
extern char *copyright_string PARAMS ((void));
extern char *license_string PARAMS ((void));

extern void syntax_error PARAMS ((Obj *x, char *msg));
extern void type_error PARAMS ((Obj *x, char *msg));
extern void interp_form PARAMS ((Module *module, Obj *form));
extern void interp_game_module PARAMS ((Obj *form, Module *module));
extern void do_module_variants PARAMS ((Module *module, Obj *lis));
extern void fill_in_side PARAMS ((Side *side, Obj *props, int userdata));
extern void read_warning PARAMS ((char *str, ...));
extern int utype_from_name PARAMS ((char *str));
extern int utype_from_symbol PARAMS ((Obj *sym));

extern void init_actions PARAMS ((void));
extern int can_move_via_conn PARAMS ((Unit *unit, int nx, int ny));
extern int can_occupy_conn PARAMS ((Unit *unit, int nx, int ny, int nz));
extern int move_unit PARAMS ((Unit *unit, int nx, int ny));
extern int can_move_at_all PARAMS ((Unit *unit));
extern int in_blocking_zoc PARAMS ((Unit *unit, int x, int y, int z));
extern int unit_blockable_by PARAMS ((Unit *unit, Unit *unit2));
extern int total_move_cost PARAMS ((int u, int u2, int x1, int y1, int z1, int x2, int y2, int z2));
extern int zoc_move_cost PARAMS ((Unit *unit, int ox, int oy, int oz));
extern int maybe_react_to_move PARAMS ((Unit *unit, int ox, int oy));
extern void consume_move_supplies PARAMS ((Unit *unit));
extern int sides_allow_entry PARAMS ((Unit *unit, Unit *transport));
extern int total_entry_cost PARAMS ((int u1, int u3, int x1, int y1, int z1, int u2, int x2, int y2, int z2));
extern int transfer_supply PARAMS ((Unit *from, Unit *to, int m, int amount));
extern void adjust_tech_crossover PARAMS ((Side *side, int u));
extern void adjust_tooling_crossover PARAMS ((Unit *unit, int u2));
extern void garrison_unit PARAMS ((Unit *unit, Unit *unit2));
extern void make_unit_complete PARAMS ((Unit *unit));
extern void distribute_material PARAMS ((Unit *unit, int m, int amt));
extern int execute_action PARAMS ((Unit *unit, Action *action));
extern int can_have_enough_acp PARAMS ((Unit *unit, int acp));
extern int has_enough_acp PARAMS ((Unit *unit, int acp));
extern int has_supply_to_act PARAMS ((Unit *unit));
extern void use_up_acp PARAMS ((Unit *unit, int acp));
extern int people_surrender_chance PARAMS ((int u, int x, int y));
extern void detonate_on_approach_around PARAMS ((Unit *unit));
extern int unit_speed PARAMS ((Unit *unit, int nx, int ny));
extern int zoc_range PARAMS ((Unit *unit, int u2));
extern int damaged_value PARAMS ((Unit *unit, Obj *effect, int maxval));

extern int can_research PARAMS ((Unit *unit));
extern int type_can_research PARAMS ((int u));
extern int can_toolup PARAMS ((Unit *unit));
extern int type_can_toolup PARAMS ((int u));
extern int can_create PARAMS ((Unit *unit));
extern int type_can_create PARAMS ((int u));
extern int can_complete PARAMS ((Unit *unit));
extern int type_can_complete PARAMS ((int u));
extern int can_repair PARAMS ((Unit *unit));
extern int type_can_repair PARAMS ((int u));
extern int can_change_type PARAMS ((Unit *unit));
extern int type_can_change_type PARAMS ((int u));
extern int can_disband PARAMS ((Unit *unit));
extern int type_can_disband PARAMS ((int u));
extern int side_can_disband PARAMS ((Side *side, Unit *unit));
extern int can_add_terrain PARAMS ((Unit *unit));
extern int type_can_add_terrain PARAMS ((int u));
extern int can_remove_terrain PARAMS ((Unit *unit));
extern int type_can_remove_terrain PARAMS ((int u));
extern int any_construction_possible PARAMS ((void));
extern char *action_desig PARAMS ((Action *act));

extern int can_attack PARAMS ((Unit *unit));
extern int type_can_attack PARAMS ((int u));
extern int can_fire PARAMS ((Unit *unit));
extern int type_can_fire PARAMS ((int u));
extern int type_can_capture PARAMS ((int u));
extern int can_detonate PARAMS ((Unit *unit));

extern int found_blocking_elevation PARAMS ((int u, int ux, int uy, int uz, int u2, int u2x, int u2y, int u2z));
extern int enough_ammo PARAMS ((Unit *unit, Unit *other));
extern void reckon_damage PARAMS ((void));
extern void reckon_damage_around PARAMS ((int x, int y, int r));
extern void damage_unit PARAMS ((Unit *unit, enum damage_reasons reason));
extern int capture_chance PARAMS ((int u, int u2, Side *side2));
extern void capture_unit PARAMS ((Unit *unit, Unit *pris));
extern int detonate_unit PARAMS ((Unit *unit, int x, int y, int z));

extern int multiply_dice PARAMS ((int dice, int mult));
extern void damage_terrain PARAMS ((int u, int x, int y));

extern int run_game PARAMS ((int maxactions));
extern void test_for_game_start PARAMS ((void));
extern void test_for_game_end PARAMS ((void));
extern int damaged_acp PARAMS ((Unit *unit, Obj *effect));
extern int all_sides_finished PARAMS ((void));
extern void finish_turn PARAMS ((Side *side));
extern void check_realtime PARAMS ((void));
extern int exceeded_rt_for_game PARAMS ((void));
extern int exceeded_rt_per_turn PARAMS ((void));
extern void compute_acp PARAMS ((Unit *unit));
extern int units_still_acting PARAMS ((Side *side));
extern void resign_game PARAMS ((Side *side, Side *side2));
extern int realtime_game PARAMS ((void));
extern int all_others_willing_to_save PARAMS ((Side *side));
extern int all_others_willing_to_quit PARAMS ((Side *side));
extern void end_the_game PARAMS ((void));
extern void change_people_side_around PARAMS ((int x, int y, int u, Side *side));
extern void init_random_events PARAMS ((void));
extern void maybe_starve PARAMS ((Unit *unit, int partial));
extern int people_surrender_chance PARAMS ((int u, int x, int y));
extern int unit_priority PARAMS ((Unit *unit));
extern int num_people_at PARAMS ((int x, int y));

extern int request_additional_side PARAMS ((char *playerspec));
extern void parse_player_spec PARAMS ((Player *player, char *spec));

extern int get_synth_method_uses PARAMS ((int methkey, int *calls, int *runs));

extern int make_fractal_terrain PARAMS ((int calls, int runs));
extern int make_random_terrain PARAMS ((int calls, int runs));
extern int make_earthlike_terrain PARAMS ((int calls, int runs));
extern int make_maze_terrain PARAMS ((int calls, int runs));
extern int name_geographical_features PARAMS ((int calls, int runs));
extern void add_edge_terrain PARAMS ((void));

extern int make_countries PARAMS ((int calls, int runs));
extern int make_independent_units PARAMS ((int calls, int runs));

extern int make_rivers PARAMS ((int calls, int runs));

extern int make_roads PARAMS ((int calls, int runs));

extern Obj *make_namer PARAMS ((Obj *sym, Obj *meth));

extern void init_namers PARAMS ((void));
extern void make_up_side_name PARAMS ((Side *side));
extern int name_in_use PARAMS ((Side *side, char *str));
extern int name_units_randomly PARAMS ((int calls, int runs));
extern char *unit_namer PARAMS ((Unit *unit));
extern char *propose_unit_name PARAMS ((Unit *unit));
extern void make_up_unit_name PARAMS ((Unit *unit));
extern void assign_unit_number PARAMS ((Unit *unit));
extern char *run_namer PARAMS ((Obj *namer));
extern char *name_from_grammar PARAMS ((Obj *grammar));
extern void gen_name PARAMS ((Obj *nonterm, Obj *rules, int depth, char *rslt));
extern void gen_from_rule PARAMS ((Obj *rule, Obj *rules, int depth, char *rslt));

extern void init_nlang PARAMS ((void));
extern void pad_out PARAMS ((char *buf, int n));
extern char *short_side_title PARAMS ((Side *side));
extern char *shortest_side_title PARAMS ((Side *side2, char *buf));
extern char *rel_short_side_title PARAMS ((Side *side, Side *side2, int n));
extern char *rel_long_side_title PARAMS ((Side *side, Side *side2));
extern char *narrative_side_desc PARAMS ((Side *side, Side *side2));
extern char *narrative_side_desc_first PARAMS ((Side *side, Side *side2));
extern char *long_player_title PARAMS ((char *buf, Player *player, char *thisdisplayname));
extern char *short_player_title PARAMS ((char *buf, Player *player, char *thisdisplayname));
extern void side_and_type_name PARAMS ((char *buf, Side *side, int u, Side *side2));
extern char *unit_handle PARAMS ((Side *side, Unit *unit));
extern char *short_unit_handle PARAMS ((Unit *unit));
extern void name_or_number PARAMS ((Unit *unit, char *buf));
extern void construction_desc PARAMS ((char *buf, Unit *unit, int u));
extern void constructible_desc PARAMS ((char *buf, Side *side, int u, Unit *unit));
extern int est_completion_time PARAMS ((Unit *unit, int u2));
extern void historical_event_date_desc PARAMS ((HistEvent *hevt, char *buf));
extern void historical_event_desc PARAMS ((Side *side, HistEvent *hevt, char *buf));
extern char *action_result_desc PARAMS ((int rslt));
extern void linear_desc PARAMS ((char *buf, int x, int y));
extern void elevation_desc PARAMS ((char *buf, int x, int y));
extern char *feature_desc PARAMS ((Feature *feature, char *buf));
extern char *feature_name_at PARAMS ((int x, int y));
extern void temperature_desc PARAMS ((char *buf, int x, int y));
extern void hp_desc PARAMS ((char *buf, Unit *unit, int label));
extern void acp_desc PARAMS ((char *buf, Unit *unit, int label));
extern void plan_desc PARAMS ((char *buf, Unit *unit));
extern void task_desc PARAMS ((char *buf, Task *task));
extern void time_desc PARAMS ((char *buf, int time, int maxtime));
extern char *summarize_units PARAMS ((char *buf, int *ucnts));
extern char *exit_commentary PARAMS ((Side *side));
extern char *ordinal_suffix PARAMS ((int n));
extern char *plural_form PARAMS ((char *word));
extern char *make_text PARAMS ((char *buf, Obj *maker, long a1, long a2, long a3, long a4));
extern char *absolute_date_string PARAMS ((int date));
extern char *relative_date_string PARAMS ((int date, int base));
extern void write_unit_record PARAMS ((FILE *fp, Side *side));
extern void write_side_results PARAMS ((FILE *fp, Side *side));
extern void write_combat_results PARAMS ((FILE *fp, Side *side));
extern void dice_desc PARAMS ((char *buf, int dice));
extern char *past_unit_handle PARAMS ((Side *side, PastUnit *past_unit));
extern char *short_past_unit_handle PARAMS ((PastUnit *past_unit));
extern void past_name_or_number PARAMS ((PastUnit *past_unit, char *buf));
extern void record_missing_image PARAMS ((int typtyp, char *str));
extern int missing_images PARAMS ((char *buf));
extern void notify_all_of_resignation PARAMS ((Side *side, Side *side2));
extern int short_side_title_plural_p PARAMS ((Side *side));
extern char *short_side_title_with_adjective PARAMS ((Side *side, char *adjective));

extern void set_initial_date PARAMS ((char *str));
extern long parse_date PARAMS ((char *datestr));
extern int find_event_type PARAMS ((Obj *sym));
extern char *side_score_desc PARAMS ((char *buf, Side *side, Scorekeeper *sk));

extern void doublecheck_state PARAMS ((Side *side));
extern long extension_value PARAMS ((Obj *extensions, char *key, long dflt));
extern void set_u_internal_name PARAMS ((int u, char *s));
extern void set_u_type_name PARAMS ((int u, char *s));
extern void set_m_type_name PARAMS ((int m, char *s));
extern void set_t_type_name PARAMS ((int t, char *s));
extern int coerce_to_side_id PARAMS ((Obj *x));
extern Side *coerce_to_side PARAMS ((Obj *x));
extern int coerce_to_unit_id PARAMS ((Obj *x));
extern Unit *coerce_to_unit PARAMS ((Obj *x));

extern void init_write PARAMS ((void));
extern int write_entire_game_state PARAMS ((char *fname));
extern int write_game_module PARAMS ((Module *module));

extern int total_gain PARAMS ((Side *side, int u));
extern int total_loss PARAMS ((Side *side, int u));

extern int lookup_plan_type PARAMS ((char *name));
extern int lookup_task_type PARAMS ((char *name));
extern int lookup_goal_type PARAMS ((char *name));

extern void set_play_rate PARAMS ((int slow, int fast));
extern int n_ms_elapsed PARAMS ((int n));
extern void record_ms PARAMS ((void));
extern int all_human_only_sides_finished PARAMS ((void));

extern int get_synth_method_uses PARAMS ((int methkey, int *calls, int *runs));
extern void set_initial_date PARAMS ((char *str));
extern int turns_between PARAMS ((char *str1, char *str2));
extern int visible_to PARAMS ((Unit *unit, Unit *unit2));
