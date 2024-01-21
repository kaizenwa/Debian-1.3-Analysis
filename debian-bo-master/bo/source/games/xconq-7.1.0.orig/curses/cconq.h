/* Definitions for the curses interface to Xconq.
   Copyright (C) 1986, 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include <curses.h>

#include "imf.h"
#include "ui.h"

/* Escape is the standard abort character. */

#define ESCAPE '\033'

/* Backspace is for fixing strings being entered. */

#define BACKSPACE '\010'

/* ^@ does a "transparent redraw" - can be done in middle of another
   command for instance. */

#define REDRAW '\000'

/* A "window" is merely a non-overlapping rectangle of character
   positions. */

struct ccwin {
  int x, y;
  int w, h;
};

/* The program can be in a number of different "modes", which affect
   both the appearance of the screen and the interpretation of input. */

enum mode {
    SURVEY,
    MOVE,
    HELP,
    MORE,
    PROMPT,
    PROMPTXY
  };

enum listsides {
    ourside,
    ourallies,
    allsides,
    numlistsides
  };

extern Side *dside;
extern int use_both_chars;
extern int follow_action;
extern int curx, cury;
extern int tmpcurx, tmpcury;
extern Unit *curunit;
extern Unit *tmpcurunit;
extern enum mode mode;
extern enum mode prevmode;
extern int itertime;
extern char inpch;
extern int prefixarg;
extern char *ustr;
extern int *uvec;
extern int *bvec;
extern int nw, nh;
extern int lastx, lasty;
extern char *text1;
extern char *text2;
extern int reqstrbeg;
extern int reqstrend;
extern struct ccwin *helpwin;
extern HelpNode *cur_help_node;
extern HelpNode *help_help_node;
extern HelpNode *commands_help_node;
extern HelpNode *topics_help_node;
extern struct ccwin *datewin;
extern struct ccwin *sideswin;
extern struct ccwin *toplineswin;
extern struct ccwin *clockwin;
extern struct ccwin *mapwin;
extern struct ccwin *listwin;
extern struct ccwin *closeupwin;
extern struct ccwin *sidecloseupwin;
extern int mw, mh;
extern int infoh;
extern int vx, vy;
extern int vw, vh;
extern int vw2, vh2;
extern VP *mvp;
extern int lastvcx, lastvcy;
extern int lw, lh;
extern int sh;
extern int drawterrain;
extern int drawunits;
extern int drawnames;
extern int drawpeople;
extern enum listsides listsides;
extern int test;
extern int value;
extern int sorton;
extern int sortorder;
extern int active;
extern char *dashbuffer;

extern void init_display PARAMS ((void));
extern void init_interaction PARAMS ((void));
extern int wait_for_char PARAMS ((void));
extern void maybe_handle_input PARAMS ((int));
extern void interpret_input PARAMS ((void));
extern void do_dir_2 PARAMS ((int dir, int n));
extern void move_survey PARAMS ((int x, int y));
extern void put_on_screen PARAMS ((int x, int y));

extern int ask_bool PARAMS ((char *question, int dflt));
extern int ask_unit_type PARAMS ((char *question, short *possibles));
extern int ask_terrain_type PARAMS ((char *question, short *possibles));
extern int ask_position PARAMS ((char *prompt, int *xp, int *yp));
extern void save_cur PARAMS ((void));
extern void restore_cur PARAMS ((void));
extern int ask_string PARAMS ((char *prompt, char *dflt, char **strp));
extern Side *ask_side PARAMS ((char *prompt, Side *dflt));
extern int ask_unit PARAMS ((char *prompt, Unit **unitp));
extern int ask_direction PARAMS ((char *prompt, int *dirp));
extern void make_current PARAMS ((Unit *unit));
extern void make_current_at PARAMS ((int x, int y));
extern void interpret_help PARAMS ((void));

extern void exit_cconq PARAMS ((void));

extern struct ccwin *create_window PARAMS ((int x, int y, int w, int h));

extern int in_middle PARAMS ((int x, int y));
extern void set_map_viewport PARAMS ((void));
extern void set_scroll PARAMS ((void));

extern void redraw PARAMS ((void));
extern void show_toplines PARAMS ((void));
extern void clear_toplines PARAMS ((void));
extern void show_closeup PARAMS ((void));
extern void show_map PARAMS ((void));
extern void draw_row PARAMS ((int x, int y, int len));
extern void show_game_date PARAMS ((void));
extern void show_clock PARAMS ((void));
extern void show_side_list PARAMS ((void));
extern void show_list PARAMS ((void));
extern void show_cursor PARAMS ((void));
extern void show_help PARAMS ((void));

extern void clear_window PARAMS ((struct ccwin *win));

#if 0
extern void draw_mushroom PARAMS ((int x, int y, int i));
#endif
extern int draw_text PARAMS ((struct ccwin *win, int x, int y, char *str));
extern int cur_at PARAMS ((struct ccwin *win, int x, int y));

extern void cycle_list_type PARAMS ((void));
extern void cycle_list_filter PARAMS ((void));
extern void cycle_list_order PARAMS ((void));

extern int auto_attack_on_move PARAMS ((Unit *, Unit *));

extern Unit *find_next_and_look PARAMS ((void));

extern void execute_command PARAMS ((void));
extern void describe_commands PARAMS ((int arg, char *key, char *buf));

extern void xbeep PARAMS ((void));
