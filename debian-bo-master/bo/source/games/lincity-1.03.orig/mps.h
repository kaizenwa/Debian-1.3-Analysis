// mps.h   part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

extern struct TYPE main_types[];
extern struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int mappointpol[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int mappoint_stats_flag;
extern char type_string[][MAPPOINT_STATS_W/8];
extern int main_screen_originx,main_screen_originy;
extern char *select_button_graphic[];
extern int select_button_bul_cost[],sbut[];
extern int mini_screen_port_x,mini_screen_port_y,mini_screen_flags;
 
