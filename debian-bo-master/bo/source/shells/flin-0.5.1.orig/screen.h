#if !defined (_SCREEN_H)
#   define _SCREEN_H

#include "parse.h"

void init_scr();
void close_scr();

void display_list(struct menu *menu_list);
void write_str(char *s);
void clear_scr();
int readchar();
int get_args(char *args, char *title);

#endif
