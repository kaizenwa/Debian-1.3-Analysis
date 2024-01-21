#ifndef _SLRN_SLRN_H_
#define _SLRN_SLRN_H_
#include <slang.h>

#define SLRN_VERSION "0.9.3.2"
extern char *Slrn_Version;

extern int Slrn_TT_Initialized;
extern void slrn_quit (int);

extern int slrn_handle_interrupts (void);

extern void slrn_set_display_state (int);
#define SLRN_SMG_INIT	1
#define SLRN_TTY_INIT	2

extern void slrn_push_suspension (int);
extern void slrn_pop_suspension (void);

extern int  slrn_get_new_news (int, int);
extern char *Slrn_Newsrc_File;
#if 0
extern void (*Slrn_Hangup_Hook) (int);
extern void (*Slrn_Winch_Function) (void);
#endif

typedef struct
{
   SLKeyMap_List_Type *keymap;
   void (*redraw_fun) (void);
   void (*sigwinch_fun) (int, int);
   void (*hangup_fun) (int);
   void (*enter_mode_hook)(void);

#define SLRN_ARTICLE_MODE	1
#define SLRN_GROUP_MODE		2
   int mode;
}
Slrn_Mode_Type;

extern void slrn_switch_to_mode (Slrn_Mode_Type *);
extern Slrn_Mode_Type *Slrn_Current_Mode;

extern void slrn_digit_arg (void);
extern void slrn_set_prefix_argument (int);
extern void slrn_repeat_last_key (void);
extern void slrn_call_command (char *);

extern long slrn_date_to_order_parm (char *);
extern int slrn_parse_helpfile (char *);
extern void slrn_smg_refresh (void);
extern void slrn_enable_mouse (int);
extern void slrn_init_hangup_signals (int);
extern int Slrn_Use_Mouse;

extern char *Slrn_Lib_Dir;
extern int Slrn_Batch;

#define HEADER_COLOR	1
#define GROUP_COLOR	2
#define SUBJECT_COLOR	3
#define AUTHOR_COLOR	4
#define ERROR_COLOR	5
#define CURSOR_COLOR	6
#define ARTICLE_COLOR	7
#define TREE_COLOR	8
#define QUOTE_COLOR	9
#define SIGNATURE_COLOR 10
#define THREAD_NUM_COLOR 11
#define HIGH_SCORE_COLOR 12
#define MENU_PRESS_COLOR 13
#define HEADER_NUMBER_COLOR 14
#define GROUP_DESCR_COLOR 15
#define GROUPLENS_DISPLAY_COLOR 16
#define SLRN_TILDE_COLOR	17
#define SLRN_HEADER_KEYWORD_COLOR	18
/* Colors below here are inverse video on B+W terminals. */
#define STATUS_COLOR	20
#define MENU_COLOR	21

#endif				       /* _SLRN_SLRN_H_ */
