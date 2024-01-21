// screen.h   part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

extern struct TYPE main_types[];
extern struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int mappointpol[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointoldtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int sbut[];
extern char progbox[(PROGBOXW+16)*(PROGBOXH+16)];
extern char message_path[],colour_pal_file[];

extern int mappoint_stats_flag,reset_mouse_flag,mini_screen_flags;
extern int mini_screen_port_x,mini_screen_port_y;
extern char type_string[][MAPPOINT_STATS_W/8];
extern int main_screen_originx,main_screen_originy;
extern char *select_button_graphic[];
// extern char select_button_message[][100];
extern int select_button_cost[];
extern int select_button_cost_mul[];
extern int select_button_bul_cost[];
extern int select_button_type[];
extern int select_button_tech[];
extern int select_button_tflag[];
extern int select_button_help_flag[];
extern char select_button_help[NUMOF_GROUPS][20];
extern int screen_refresh_flag;
extern int group_colour[];
extern int population,starving_population,food_in_markets,housed_population;
extern int jobs_in_markets,coal_in_markets,goods_in_markets;
extern int ore_in_markets,steel_in_markets;
extern int sust_dig_ore_coal_count,sust_port_count,sust_old_money;
extern int sust_old_money_count,sust_old_population;
extern int sust_old_population_count,sustain_flag;
extern int sust_old_tech,sust_old_tech_count,sust_fire_count;
extern int substationx[],substationy[],numof_substations;

extern float gamma_correct_red,gamma_correct_green,gamma_correct_blue;

extern int unemployed_population,max_pop_ever;
extern int mouse_type,cs_mouse_button,cs_mouse_button_repeat;
extern int cs_mouse_button_delay,real_time;
extern int income_tax,coal_tax,income_tax_rate,coal_tax_rate;
extern int unemployment_cost,transport_cost,total_money,school_cost;
extern int other_cost,university_cost,recycle_cost,windmill_cost;
extern int cricket_cost,dole_rate,transport_cost_rate;
extern int goods_tax,goods_tax_rate,fire_cost,ly_fire_cost;
extern int people_pool,unnat_deaths,deaths_cost,rocket_pad_cost;
extern int ly_university_cost,ly_recycle_cost,ly_school_cost,ly_deaths_cost;
extern int ly_health_cost,ly_rocket_pad_cost,ly_interest,ly_windmill_cost;
extern int ly_cricket_cost;

extern int monthgraph_pop[MAPPOINT_STATS_W];
extern int monthgraph_starve[MAPPOINT_STATS_W];
extern int monthgraph_nojobs[MAPPOINT_STATS_W];
extern int monthgraph_ppool[MAPPOINT_STATS_W];
extern int tpopulation,tstarving_population,tunemployed_population;
extern int export_tax,export_tax_rate,import_cost,import_cost_rate;

extern char *up_pbar1_graphic,*up_pbar2_graphic;
extern char *down_pbar1_graphic,*down_pbar2_graphic,*pop_pbar_graphic;
extern char *tech_pbar_graphic,*food_pbar_graphic,*jobs_pbar_graphic;
extern char *money_pbar_graphic,*coal_pbar_graphic,*goods_pbar_graphic;
extern char *ore_pbar_graphic,*steel_pbar_graphic;

extern char *pause_button1_off,*pause_button2_off;
extern char *pause_button1_on,*pause_button2_on;
extern char *fast_button1_off,*fast_button2_off;
extern char *fast_button1_on,*fast_button2_on;
extern char *med_button1_off,*med_button2_off;
extern char *med_button1_on,*med_button2_on;
extern char *slow_button1_off,*slow_button2_off;
extern char *slow_button1_on,*slow_button2_on;
extern char *results_button1,*results_button2;
extern char *toveron_button1,*toveron_button2;
extern char *toveroff_button1,*toveroff_button2;
#ifdef LC_X11
extern char *confine_button,*unconfine_button;
#endif
extern char *ms_pollution_button_graphic,*ms_normal_button_graphic;
extern char *ms_fire_cover_button_graphic,*ms_health_cover_button_graphic;
extern char *ms_cricket_cover_button_graphic;
extern char *ms_ub40_button_graphic,*ms_coal_button_graphic;
extern char *checked_box_graphic,*unchecked_box_graphic;
extern char *ms_starve_button_graphic,*ms_ocost_button_graphic;
extern char *ms_power_button_graphic;
extern char market_cb_gbuf[];
extern int market_cb_flag,mcbx,mcby,market_cb_drawn_flag;
extern int port_cb_flag,port_cb_drawn_flag;
extern int tech_level,highest_tech_level;
extern int power_made,power_used,coal_made,coal_used;
extern int goods_made,goods_used,ore_made,ore_used;
extern int diffgraph_power[MAPPOINT_STATS_W],diffgraph_coal[MAPPOINT_STATS_W];
extern int diffgraph_goods[MAPPOINT_STATS_W],diffgraph_ore[MAPPOINT_STATS_W];
extern int diffgraph_population[MAPPOINT_STATS_W],diff_old_population;
extern int total_time;
extern int health_cost;
extern char *help_button_graphic,*quit_button_graphic;
extern char *load_button_graphic,*save_button_graphic;
extern int help_flag,load_flag,save_flag;
extern int db_yesbox_x1,db_yesbox_x2,db_yesbox_y1,db_yesbox_y2;
extern int db_nobox_x1,db_nobox_x2,db_nobox_y1,db_nobox_y2;
extern int db_flag,db_yesbox_clicked,db_nobox_clicked;
extern char okmessage[22][74];
extern int db_okbox_x1,db_okbox_x2,db_okbox_y1,db_okbox_y2,db_okflag;
extern int db_okbox_clicked,suppress_ok_buttons;

extern int pbar_pops[12],pbar_pop_oldtot,pbar_pop_olddiff;
extern int pbar_techs[12],pbar_tech_oldtot,pbar_tech_olddiff;
extern int pbar_foods[12],pbar_food_oldtot,pbar_food_olddiff;
extern int pbar_jobs[12],pbar_jobs_oldtot,pbar_jobs_olddiff;
extern int pbar_coal[12],pbar_coal_oldtot,pbar_coal_olddiff;
extern int pbar_goods[12],pbar_goods_oldtot,pbar_goods_olddiff;
extern int pbar_ore[12],pbar_ore_oldtot,pbar_ore_olddiff;
extern int pbar_steel[12],pbar_steel_oldtot,pbar_steel_olddiff;
extern int pbar_money[12],pbar_money_oldtot,pbar_money_olddiff;
extern int rockets_launched_success,rockets_launched;
extern int numof_shanties;
extern int ex_tax_dis[];

#ifdef USE_PIXMAPS
extern Pixmap icon_pixmap[];
#endif
