#include "slang.h"
#define move SLsmg_gotorc
#define clreol SLsmg_erase_eol
#define printw SLsmg_printf
#define mvprintw(x, y, z) SLsmg_gotorc(x, y); SLsmg_printf(z)
#define COLS SLtt_Screen_Cols
#define LINES SLtt_Screen_Rows
#define clrtobot SLsmg_erase_eos
#define clrtoeol SLsmg_erase_eol
#define standout SLsmg_reverse_video
#define standend  SLsmg_normal_video
#define addch SLsmg_write_char
#define addstr SLsmg_write_string
#define initscr SLtt_get_terminfo (); SLsmg_init_smg
#define refresh SLsmg_refresh
#define clear SLsmg_cls
#define erase SLsmg_cls
#define mvaddstr(y, x, s) SLsmg_gotorc(y, x); SLsmg_write_string(s)
#define touchline SLsmg_touch_lines
#define inch SLsmg_char_at
#define endwin SLsmg_reset_smg
#ifdef A_NORMAL
#undef A_NORMAL
#endif
#define A_NORMAL 0
