// main.h    part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

struct TYPE main_types[NUM_OF_TYPES];
struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
int mappointpol[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
short mappointoldtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
int sbut[NUMOF_SELECT_BUTTONS];
int group_count[NUMOF_GROUPS];
int lc_mouse_type;
char progbox[(PROGBOXW+16)*(PROGBOXH+16)];
int dummy123[100];
char colour_pal_file[256],opening_pic[256],graphic_path[256],fontfile[256];
char helppath[256],message_path[256],given_scene[256],lc_save_dir[256];

int no_init_help,pix_double=0;
int selected_type; // the type selected by buttons to draw in main win
int selected_button; // the button selected. So we can work out the cost
int selected_type_cost;
int select_button_type[NUMOF_GROUPS]; // button number => selected type
char *select_button_graphic[NUMOF_GROUPS];
// char select_button_message[NUMOF_GROUPS][100];
int select_button_cost[NUMOF_GROUPS];
int select_button_cost_mul[NUMOF_GROUPS];
int select_button_bul_cost[NUMOF_GROUPS];
int select_button_tech[NUMOF_GROUPS];
int select_button_tflag[NUMOF_GROUPS];
int select_button_help_flag[NUMOF_GROUPS];
char select_button_help[NUMOF_GROUPS][20];
char type_string[NUMOF_GROUPS][MAPPOINT_STATS_W/8];
int old_selected_button=0;
int mappoint_array_x[WORLD_SIDE_LEN],mappoint_array_y[WORLD_SIDE_LEN];
int mappoint_stats_flag=0,mappoint_stats_time=0;
int mini_screen_flags=0,mini_screen_time=0;
int mini_screen_port_x,mini_screen_port_y;
int fire_chance[NUMOF_GROUPS];
int real_start_time,real_time;
// int buf1[10];
struct timeval lc_timeval;
// int buf2[10];
int numof_shanties,numof_communes;
float gamma_correct_red=0.0,gamma_correct_green=0.0,gamma_correct_blue=0.0;

int cs_mouse_x,cs_mouse_y,cs_mouse_button; // current mouse status
int cs_mouse_shifted=0; // shift key pressed with mouse.
int cs_mouse_xmax,cs_mouse_ymax,omx,omy,mox=10,moy=10;
int cs_mouse_button_repeat,mouse_hide_count,cs_mouse_button_delay;
int kmousex,kmousey,kmouse_val,reset_mouse_flag,mt_flag=0;
int overwrite_transport_flag=0;
int sust_dig_ore_coal_tip_flag=0,sust_port_flag=0,sustain_flag=0;
int sust_dig_ore_coal_count=0,sust_port_count=0,sust_old_money=0;
int sust_old_money_count=0,sust_old_population=0,sust_old_population_count=0;
int sust_old_tech=0,sust_old_tech_count=0,sust_fire_count=0;

char under_square_mouse_pointer_top[20*2*4];
char under_square_mouse_pointer_left[18*2*4];
char under_square_mouse_pointer_right[18*2*4];
char under_square_mouse_pointer_bottom[20*2*4];
int mouse_type=MOUSE_TYPE_NORMAL;

int mt_start_posx,mt_start_posy,mt_current_posx,mt_current_posy;
int main_screen_originx,main_screen_originy;

int total_time=0;
int population,starving_population,food_in_markets,jobs_in_markets;
int housed_population;
int coal_in_markets,goods_in_markets,ore_in_markets,steel_in_markets;
int unemployed_population,people_pool;
// int dummy1[10];
int substationx[MAX_NUMOF_SUBSTATIONS],substationy[MAX_NUMOF_SUBSTATIONS];
int numof_substations=0;
int marketx[MAX_NUMOF_MARKETS],markety[MAX_NUMOF_MARKETS],numof_markets=0;
int numof_health_centres,max_pop_ever=0,total_evacuated=0,total_births=0;

int screen_refresh_flag=1;

int group_colour[]={
	GROUP_BARE_COLOUR,GROUP_POWER_LINE_COLOUR,GROUP_POWER_SOURCE_COLOUR
	,GROUP_SUBSTATION_COLOUR,GROUP_RESIDENCE_COLOUR
	,GROUP_ORGANIC_FARM_COLOUR,GROUP_MARKET_COLOUR,GROUP_TRACK_COLOUR
	,GROUP_COALMINE_COLOUR,GROUP_RAIL_COLOUR
	,GROUP_POWER_SOURCE_COAL_COLOUR,GROUP_ROAD_COLOUR
	,GROUP_INDUSTRY_L_COLOUR,GROUP_UNIVERSITY_COLOUR
	,GROUP_COMMUNE_COLOUR,GROUP_OREMINE_COLOUR
	,GROUP_TIP_COLOUR,GROUP_EX_PORT_COLOUR
	,GROUP_INDUSTRY_H_COLOUR,GROUP_PARKLAND_COLOUR
	,GROUP_RECYCLE_COLOUR,GROUP_WATER_COLOUR
	,GROUP_HEALTH_COLOUR,GROUP_ROCKET_COLOUR
	,GROUP_WINDMILL_COLOUR,GROUP_MONUMENT_COLOUR
	,GROUP_SCHOOL_COLOUR,GROUP_BLACKSMITH_COLOUR
	,GROUP_MILL_COLOUR,GROUP_POTTERY_COLOUR
	,GROUP_FIRESTATION_COLOUR,GROUP_CRICKET_COLOUR
	,GROUP_BURNT_COLOUR,GROUP_SHANTY_COLOUR};

int income_tax,coal_tax,total_money=0,income_tax_rate,coal_tax_rate;
int unemployment_cost,transport_cost;
int other_cost,university_cost,recycle_cost,school_cost,windmill_cost;
int dole_rate,transport_cost_rate,cricket_cost;
int goods_tax,goods_tax_rate;
int export_tax,export_tax_rate,import_cost,import_cost_rate;
int tech_level,highest_tech_level,unnat_deaths,deaths_cost;
int health_cost,rocket_pad_cost,fire_cost,ly_fire_cost;
int ly_university_cost,ly_recycle_cost,ly_school_cost,ly_deaths_cost;
int ly_health_cost,ly_rocket_pad_cost,ly_interest,ly_windmill_cost;
int ly_cricket_cost;
char *months[]= {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"
		,"Sep","Oct","Nov","Dec" };

int monthgraph_pop[MAPPOINT_STATS_W],monthgraph_starve[MAPPOINT_STATS_W];
int monthgraph_nojobs[MAPPOINT_STATS_W],monthgraph_ppool[MAPPOINT_STATS_W];
int tpopulation,tstarving_population,tunemployed_population;
int total_pollution_deaths=0,total_starve_deaths=0,total_unemployed_days=0;
int total_unemployed_years=0;
float pollution_deaths_history=0.0,starve_deaths_history=0.0;
float unemployed_history=0.0;

char *up_pbar1_graphic,*up_pbar2_graphic;
char *down_pbar1_graphic,*down_pbar2_graphic,*pop_pbar_graphic;
char *tech_pbar_graphic,*food_pbar_graphic,*jobs_pbar_graphic;
char *money_pbar_graphic,*coal_pbar_graphic,*goods_pbar_graphic;
char *ore_pbar_graphic,*steel_pbar_graphic;
char *pause_button1_off,*pause_button2_off;
char *pause_button1_on,*pause_button2_on;
char *fast_button1_off,*fast_button2_off;
char *fast_button1_on,*fast_button2_on;
char *med_button1_off,*med_button2_off;
char *med_button1_on,*med_button2_on;
char *slow_button1_off,*slow_button2_off;
char *slow_button1_on,*slow_button2_on;
char *results_button1,*results_button2;
char *toveron_button1,*toveron_button2;
char *toveroff_button1,*toveroff_button2;
#ifdef LC_X11
char *confine_button,*unconfine_button;
int confine_flag=0;
#endif
int pause_flag=0,slow_flag=0,med_flag=0,fast_flag=0;

char *ms_pollution_button_graphic,*ms_normal_button_graphic;
char *ms_fire_cover_button_graphic,*ms_health_cover_button_graphic;
char *ms_cricket_cover_button_graphic;
char *ms_ub40_button_graphic,*ms_coal_button_graphic;
char *ms_starve_button_graphic,*ms_ocost_button_graphic;
char *ms_power_button_graphic;
char *checked_box_graphic,*unchecked_box_graphic;
char market_cb_gbuf[MARKET_CB_W*MARKET_CB_H];
int market_cb_flag=0,mcbx,mcby,market_cb_drawn_flag=0;
int port_cb_flag=0,port_cb_drawn_flag=0;
int university_intake_rate;

int waste_goods;
int power_made,power_used,coal_made,coal_used;
int goods_made,goods_used,ore_made,ore_used;
int diffgraph_power[MAPPOINT_STATS_W],diffgraph_coal[MAPPOINT_STATS_W];
int diffgraph_goods[MAPPOINT_STATS_W],diffgraph_ore[MAPPOINT_STATS_W];
int diffgraph_population[MAPPOINT_STATS_W],diff_old_population;
int rockets_launched,rockets_launched_success;
int last_built_x,last_built_y;
int help_flag,numof_help_buttons,help_history_count,help_return_val;
int block_help_exit;
int help_button_x[MAX_NUMOF_HELP_BUTTONS];
int help_button_y[MAX_NUMOF_HELP_BUTTONS];
int help_button_w[MAX_NUMOF_HELP_BUTTONS];
int help_button_h[MAX_NUMOF_HELP_BUTTONS];
char help_button_s[MAX_NUMOF_HELP_BUTTONS][MAX_LENOF_HELP_FILENAME];
char help_button_history[MAX_HELP_HISTORY][MAX_LENOF_HELP_FILENAME];
char *help_button_graphic,help_graphic[MAX_ICON_LEN];
char *quit_button_graphic,*load_button_graphic,*save_button_graphic;
int quit_flag,load_flag,save_flag,cheat_flag;
int monument_bul_flag,river_bul_flag;
int must_release_button=0,let_one_through=0;

int db_yesbox_x1,db_yesbox_x2,db_yesbox_y1,db_yesbox_y2;
int db_nobox_x1,db_nobox_x2,db_nobox_y1,db_nobox_y2;
int db_flag,db_yesbox_clicked,db_nobox_clicked;
char okmessage[22][74];
int db_okbox_x1,db_okbox_x2,db_okbox_y1,db_okbox_y2,db_okflag,db_okbox_clicked;
int suppress_ok_buttons;
int coal_survey_done;

char save_names[10][42];

int pbar_pops[12],pbar_pop_oldtot,pbar_pop_olddiff;
int pbar_techs[12],pbar_tech_oldtot,pbar_tech_olddiff;
int pbar_foods[12],pbar_food_oldtot,pbar_food_olddiff;
int pbar_jobs[12],pbar_jobs_oldtot,pbar_jobs_olddiff;
int pbar_coal[12],pbar_coal_oldtot,pbar_coal_olddiff;
int pbar_goods[12],pbar_goods_oldtot,pbar_goods_olddiff;
int pbar_ore[12],pbar_ore_oldtot,pbar_ore_olddiff;
int pbar_steel[12],pbar_steel_oldtot,pbar_steel_olddiff;
int pbar_money[12],pbar_money_oldtot,pbar_money_olddiff;

int ex_tax_dis[NUMOF_DISCOUNT_TRIGGERS]={
        EX_DISCOUNT_TRIGGER_1,
        EX_DISCOUNT_TRIGGER_2,
        EX_DISCOUNT_TRIGGER_3,
        EX_DISCOUNT_TRIGGER_4,
        EX_DISCOUNT_TRIGGER_5,
        EX_DISCOUNT_TRIGGER_6,
        };

//  X stuff --------------------------------------
                                                        
#ifdef LC_X11
int             winX, winY,mouse_button;
unsigned int    winW, winH;
disp            display;

char    *bg_color = NULL;
char dummy1[1024];
char    pixmap[(640+BORDERX*2)*(480+BORDERY*2)];
char dummy2[1024];
char    myfont[256*8];
int     verbose = FALSE;        /* display settings if TRUE */
int     stay_in_front = FALSE;  /* Try to stay in clear area of the screen. */
int     text_bg=0;
int     text_fg=255;
int     x_key_value;
int     xclip_x1,xclip_y1,xclip_x2,xclip_y2,clipping_flag=0;
int     borderx,bordery;
long unsigned int  colour_table[256];
unsigned char *open_font;
int open_font_height,suppress_next_expose=0;
Cursor pirate_cursor;

#ifdef USE_PIXMAPS
Pixmap icon_pixmap[NUM_OF_TYPES];
char icon_pixmap_flag[NUM_OF_TYPES];
#endif

#endif
