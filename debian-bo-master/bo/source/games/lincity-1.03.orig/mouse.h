// mouse.h  part of lincity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

//extern int add_a_substation(int,int);
//extern void mappoint_stats(int,int);
//extern void refresh_main_screen(void);

extern struct TYPE main_types[];
extern struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern char under_square_mouse_pointer_top[];
extern char under_square_mouse_pointer_left[];
extern char under_square_mouse_pointer_right[];
extern char under_square_mouse_pointer_bottom[];
extern char select_button_help[NUMOF_GROUPS][20];
extern int cs_mouse_x,cs_mouse_y,cs_mouse_button,cs_mouse_xmax,cs_mouse_ymax;
extern int cs_mouse_button_repeat,mouse_hide_count,cs_mouse_button_delay;
extern int kmousex,kmousey,kmouse_val,real_time;
extern int omx,omy,mox,moy,selected_type,select_button_type[];
extern int selected_button,selected_type_cost;
extern int old_selected_button;
extern int select_button_cost[];
extern int select_button_cost_mul[];
extern int select_button_bul_cost[];
extern int select_button_tflag[];
extern int select_button_help_flag[];
extern char select_button_message[][100];
extern char type_string[][MAPPOINT_STATS_W/8];
extern int mouse_type,lc_mouse_type;
extern int main_screen_originx,main_screen_originy;
extern int total_money;
extern int mappoint_stats_flag,mt_flag;
extern int people_pool;
extern int tech_level;
extern char *checked_box_graphic,*unchecked_box_graphic;
extern int market_cb_flag,mcbx,mcby,port_cb_flag;
extern int numof_health_centres;
extern int last_built_x,last_built_y;
extern int help_flag,quit_flag,load_flag,save_flag,block_help_exit;
extern int db_yesbox_x1,db_yesbox_x2,db_yesbox_y1,db_yesbox_y2;
extern int db_nobox_x1,db_nobox_x2,db_nobox_y1,db_nobox_y2;
extern int db_flag,db_yesbox_clicked,db_nobox_clicked;
extern int coal_survey_done;
extern int db_okbox_x1,db_okbox_x2,db_okbox_y1,db_okbox_y2,db_okflag;
extern int db_okbox_clicked;
extern int monument_bul_flag,river_bul_flag;
extern int sbut[];
extern int numof_shanties,numof_communes;
extern int overwrite_transport_flag;
extern int mt_start_posx,mt_start_posy,mt_current_posx,mt_current_posy;

extern char *pause_button1_off,*pause_button2_off;
extern char *pause_button1_on,*pause_button2_on;
extern char *fast_button1_off,*fast_button2_off;
extern char *fast_button1_on,*fast_button2_on;
extern char *med_button1_off,*med_button2_off;
extern char *med_button1_on,*med_button2_on;
extern char *slow_button1_off,*slow_button2_off;
extern char *slow_button1_on,*slow_button2_on;
extern char *toveron_button1,*toveron_button2;
extern char *toveroff_button1,*toveroff_button2;
extern int pause_flag,slow_flag,med_flag,fast_flag;
extern int must_release_button,help_return_val;
extern int let_one_through;
extern int mini_screen_flags;

#ifdef LC_X11
extern char *confine_button,*unconfine_button;
extern int confine_flag;
extern Cursor pirate_cursor;
#endif
