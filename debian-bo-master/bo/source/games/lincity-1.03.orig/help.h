// help.h   part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

extern int help_flag,numof_help_buttons,help_history_count,help_return_val;
extern int help_button_x[];
extern int help_button_y[];
extern int help_button_w[];
extern int help_button_h[];
extern char help_button_s[][MAX_LENOF_HELP_FILENAME];
extern char help_button_history[][MAX_LENOF_HELP_FILENAME];
extern char help_graphic[];
extern int selected_type,block_help_exit;
extern int selected_type_cost,tech_level;
extern char graphic_path[],helppath[];
extern int load_flag;
