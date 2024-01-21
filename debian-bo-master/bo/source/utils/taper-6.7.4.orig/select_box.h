/*
   Time-stamp: <96/07/19 20:18:25 yusuf>

   $Id: select_box.h,v 1.15 1996/07/27 20:42:14 yusuf Exp $
*/


#ifndef _FROM_MAIN_
  #define inline __inline__

  #include <dirent.h>
  #include <errno.h>
  #include <unistd.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <curses.h>
  #include <form.h>
  #include <fcntl.h>
  #include <sys/stat.h>
  #include "ttypes.h"
#endif

#include "memory.h"
#include "errors.h"

#define MAX_FNAME NAME_MAX

#define MB_OK 0					 /* message box values */
#define MB_YESNO 1
#define MB_WAIT 2
#define MB_OKCANCEL 3
#define MB_APPENDOVERWRITE 4
#define MB_RETRYABORT 5

#define SELECT_ABORT 0
#define SELECT_FINISHED 1
#define SELECT_TAB 2
#define SELECT_ENTER 3

#define COLOR_MAIN 1
#define COLOR_DIALOG 2				 /* color of dialog box */
#define COLOR_STATUS 3				 /* status box colors */
#define COLOR_HELP 4
#define COLOR_DIRECTORY 5
#define COLOR_FORM 6


extern char dir_cur_dir[MAX_FNAME];

typedef struct {
    _s32 top_entry, cur_entry;
    int cursor_line;
} select_details;
extern void clear_sd(select_details *sd);


extern void my_init_windows(void);     
extern WINDOW *my_newwin(int nlines, int ncols, int beginy, int beginx);
extern void my_delwin(WINDOW *win);
extern void my_delwin_all(void);
extern void my_werase(WINDOW *win, int color);
extern _s32 max(_s32 a, _s32 b);
extern _s32 min(_s32 a, _s32 b);
extern void centre(WINDOW *win, int line, char *s, int color);
extern WINDOW *status_box(WINDOW *win, char *s, int line, int create, int no_lines);
extern void close_statusbox(WINDOW *win);
extern int message_box(char *s, int type);
extern int multi_message_box(char lns[][150], int count, int type);

typedef void (*print_screen) (WINDOW *win, _s32 top, char *p_scroll);
typedef void (*print_line) (WINDOW *win, _s32 cursor_entry, int cursor_line, char refresh);
typedef int (*tag_entry) (_s32 cursor_entry);
typedef void (*delete_entry) (select_details *sd, _s32 *no_sel);
typedef void (*refresh_screen) (void);
typedef void (*help_screen) (void);
typedef void (*sr_backupset) (int in_backup);
typedef void (*sr_restoreset) (void);

extern int select_box(WINDOW *win, _s32 *no_entries,  select_details *sd,
		      print_screen ps, print_line pl, tag_entry te, delete_entry de,
		      refresh_screen rs,
		      WINDOW *sel_win, _s32 *sel_no_entries,
		      select_details *sel_sd, print_screen sel_ps,
		      print_screen asterix, select_details *fdetails,
		      char *select_help, sr_backupset save_backupset,
		      sr_restoreset rest_backupset);
extern void print_scroll_bar(WINDOW *win, _s32 no_entries, select_details *sd,
		      int s_len, int s_width, char *p_scroll);
extern int select_menu(WINDOW *win, char *menu_items[], int *in_op);


/* Stuff for directory selection */
struct direntry {	
    struct stat     info;
    struct stat     org_info;
    struct dirent   entry;
};


extern struct direntry      *directory;		 /* for the select directory */
extern struct direntry *sb_directory;
extern _s32 directory_count;			 /* routines */
extern char *global_cur_dir;
extern select_details dir_sd;
extern int dir_left_width;
extern int dir_screen_ylen;

typedef void (*modify_filename) (struct direntry *x, char *dir_name, char *prefix);
	      
extern void my_strcpy(char *dest, char *src, int maxlen);
extern void find_correct_sd(char *old_dir, select_details *sd, _s32 dir_count,
			    struct direntry *dir);
extern char *convert(char *s, _s32 num);
extern int read_dir(char *dir_name, char *prefix, modify_filename fn,
		    struct direntry **den, _s32 *den_count);
extern void get_file_type(char *s, umode_t mode);
extern void add_perm_string(char *s, char *a, int perm);
extern void print_dir(WINDOW *win, _s32 start, char *p_scroll);
extern void print_dir_line(WINDOW *win, struct direntry *entry, int line, 
			   char ref, int dir_left_width);
extern int select_file(char *start_dir, char *prefix, char *deflt, 
		       modify_filename fn, char change_dir);
extern int get_string(WINDOW *win, char *s, int maxlen, char *prompt);
extern int retryabort(char *s);
