#ifndef __MYSLANG_H
#define __MYSLANG_H

#include "../slang/slang.h"

enum {
    KEY_BACKSPACE = 400,
    KEY_END, KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT,
    KEY_HOME, KEY_A1, KEY_C1, KEY_NPAGE, KEY_PPAGE, KEY_IC,
    KEY_ENTER, KEY_DC, KEY_SCANCEL
};

#define KEY_F(x) 1000+x

#define ACS_VLINE SLSMG_VLINE_CHAR
#define ACS_HLINE SLSMG_HLINE_CHAR
#define ACS_ULCORNER SLSMG_ULCORN_CHAR
#define ACS_LLCORNER SLSMG_LLCORN_CHAR
#define ACS_URCORNER SLSMG_URCORN_CHAR
#define ACS_LRCORNER SLSMG_LRCORN_CHAR
#define ACS_LTEE 't'

#ifdef _OS_NT
#    define acs()   ;
#    define noacs() ;
#    define baudrate() 19200
#else
#    define acs()   SLsmg_set_char_set(1)
#    define noacs() SLsmg_set_char_set (0)
#    define baudrate() SLang_TT_Baud_Rate
#endif

enum {
    COLOR_BLACK, COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_BLUE,
    COLOR_MAGENTA, COLOR_CYAN, COLOR_WHITE
};

/* When using Slang with color, we have all the indexes free but
   those defined here (A_BOLD, A_UNDERLINE, A_REVERSE) */
#define A_BOLD      0x40
#define A_UNDERLINE 0x40
#define A_REVERSE   0x20

#ifndef A_NORMAL
#    define A_NORMAL    0x00
#endif

#define ACS_MAP(x) '*'
#define COLOR_PAIR(x) x
#define ERR -1
#define TRUE 1
#define FALSE 0

#define doupdate()
#define raw()
#define noraw()
#define nodelay(x,val) set_slang_delay(val)
#define noecho()
#define beep() SLtt_beep ()
#define keypad(scr,value) slang_keypad (value)

#define ungetch(x) SLang_ungetkey(x)
#define start_color()
#define touchwin(x) SLsmg_touch_lines(0, LINES)
#define reset_shell_mode slang_shell_mode
#define reset_prog_mode slang_prog_mode
#define flushinp()

void slint_goto (int y, int x);
void attrset (int color);
void set_slang_delay (int);
void slang_init (void);
void slang_done_screen (void);
void slang_prog_mode (void);
void hline (int ch, int len);
void vline (int ch, int len);
int getch (void);
void slang_keypad (int set);
void slang_shell_mode (void);
void slang_shutdown (void);
void init_pair (int, char *, char *);
/* Internal function prototypes */
void load_terminfo_keys ();

#if 0
#   define one_vline() {acs (); addch (ACS_VLINE); noacs ();}
#   define one_hline() {acs (); addch (ACS_HLINE); noacs ();}
    /* This is fast, but unusefull if ! pc_system - doesn't use
       Alt_Char_Pairs [] :( */
#else
    /* This is slow, but works well :| */ 
#   define one_vline() SLsmg_draw_object (SLsmg_get_row(), SLsmg_get_column(), slow_terminal ? ' ' : ACS_VLINE) 
#   define one_hline() SLsmg_draw_object (SLsmg_get_row(), SLsmg_get_column(), slow_terminal ? ' ' : ACS_HLINE) 
#endif    

void enable_interrupt_key ();
void disable_interrupt_key ();
#endif
