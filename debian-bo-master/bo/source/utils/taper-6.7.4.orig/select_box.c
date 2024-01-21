/*
   Time-stamp: <96/07/19 20:18:22 yusuf>

   $Id: select_box.c,v 1.19 1996/07/27 20:42:13 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: select_box.c,v 1.19 1996/07/27 20:42:13 yusuf Exp $";
#endif /* lint */



#include "select_box.h"


extern WINDOW *win_main;
WINDOW *dir_win;
struct direntry      *sb_directory;		 /* for the select directory */
struct direntry *directory;
_s32 sb_directory_count;			 /* routines */
_s32 directory_count;
select_details sb_dir_sd;
select_details dir_sd;
char *global_cur_dir;
char  dir_cur_dir[MAX_FNAME];
int sb_dir_left_width;
int dir_screen_ylen;

int open_windows;
WINDOW *open_wind[20];
void my_init_windows()
{
    int c;
    for (c=0; c<20;c++)
      open_wind[c] = NULL;
}


void my_strcpy(char *dest, char *src, int maxlen)
{
/* Copies string src to dest but up to a maximum of maxlen chars 
 * Also removes trailing spaces
 */
    char *s;
    
    strncpy(dest, src, maxlen); 
    dest[maxlen-1] = 0;
    s = &dest[strlen(dest)-1];
    while ((*s == ' ') && (s != dest)) {
	*s = 0;
	s--;
    }
}



WINDOW *my_newwin(int nlines, int ncols, int beginy, int beginx)
{
    int c;

    for (c=0; c<20; c++)
      if (open_wind[c] == NULL)
	break;
    if (c==20)
      return NULL;
    open_wind[c] = newwin(nlines, ncols, beginy, beginx);
    return open_wind[c];
}


void my_delwin(WINDOW *win)
{
    int c;

    for (c=0; c<20; c++)
      if (open_wind[c] == win)
	break;
    delwin(win);
    if (c < 20)
      open_wind[c] = NULL;
}

void my_delwin_all()
{
    int c;

    for (c=0; c<20; c++)
      if (open_wind[c]) {
	  delwin(open_wind[c]);
	  open_wind[c] = NULL;
      }
}


void my_werase(WINDOW *win, int color)
{
    int x, y;
    
    if (has_colors()) wattron(win, COLOR_PAIR(color));
    for (y=0; y<=win->_maxy; y++) {
	wmove(win, y, 0);
	for (x=0; x<=win->_maxx; x++)
	  waddch(win, ' ');
    }
    if (win->_maxy > 1)				 /* don't bother bordering */
      box(win, ACS_VLINE, ACS_HLINE);		 /* one line windows */
}


_s32 max(_s32 a, _s32 b) {
    return (a > b) ? a : b;
}


_s32 min(_s32 a, _s32 b) {
    return (a < b) ? a : b;
}


int get_string(WINDOW *win, char *s, int maxlen, char *prompt)
{
/* Prints up a box and prompts for a string 
   Returns 0 if pressed ESC, 1 otherwise
 */   
    WINDOW *mes=NULL;
    FIELD *field[2];
    FORM *form;
    int  c;

    mes = my_newwin(5, 40, (win->_maxy-5)/2, (win->_maxx-40)/2);
    keypad(mes, TRUE);
    my_werase(mes, COLOR_FORM);
    field[0] = new_field(1, 30, 2, 5, 0, 0);
    field_opts_off(field[0], O_STATIC); 
    set_field_back(field[0], A_REVERSE|COLOR_PAIR(COLOR_FORM));
    field_opts_off(field[0], O_AUTOSKIP);
    set_max_field(field[0], 0); 
    set_field_buffer(field[0], 0, s);
    field[1] = NULL;
    form = new_form(field);
    set_form_win(form, mes);
    set_form_sub(form, mes);
    post_form(form);
    if (has_colors()) wattron(mes, COLOR_PAIR(COLOR_FORM));
    box(mes, ACS_VLINE, ACS_HLINE); 
    mvwaddstr(mes, 0, 20-strlen(prompt)/2, prompt);
    curs_set(1);
    wmove(mes, 2, 5); 
    wrefresh(mes);
    while (1) {
	c = wgetch(mes);
	switch(c) {
	 case KEY_RIGHT:
	    c = REQ_NEXT_CHAR; break;
	 case KEY_LEFT:
	    c = REQ_PREV_CHAR; break;
	 case KEY_HOME:
	    c = REQ_BEG_FIELD; break;
	 case KEY_END:
	    c = REQ_END_FIELD; break;
	 case KEY_NPAGE:
	    c = REQ_NEXT_CHOICE; break;
	 case KEY_PPAGE:
	    c = REQ_PREV_CHOICE; break;
	 case KEY_DC:
	    c = REQ_DEL_CHAR; break;
	 case KEY_BACKSPACE:
	 case 127:
	    c = REQ_DEL_PREV; break;
	}
	if (c == '\r' || c == '\n' || c == KEY_F0+10) break;
	form_driver(form, c);
	wrefresh(form->w);
    }
    form_driver(form, REQ_VALIDATION);
    my_strcpy(s, field_buffer(field[0], 0), maxlen); 
    unpost_form(form);
    free_form(form);
    free_field(field[0]);
    my_delwin(mes);
    curs_set(0);
    return (c != KEY_F0+10);
}

    

void centre(WINDOW *win, int line, char *s, int color) {
    int maxs=min(win->_maxx-2, strlen(s));
    int c;
    char s1[300];
    
    strcpy(s1,s);
    if (has_colors()) wattron(win, COLOR_PAIR(color));
    s1[maxs] = 0;
    wmove(win, line, 1);
    for (c=0;c<win->_maxx-1;c++)
      waddch(win, ' ');
    mvwaddstr(win, line, (win->_maxx  - maxs)/2+1, s1);
}
    

void centre_invert(WINDOW *win, int line, char *s, int color)
{
    int maxs=min(win->_maxx-2, strlen(s));
    char s1[300];
    
    strcpy(s1,s);
    if (has_colors()) wattron(win, COLOR_PAIR(color));
    s1[win->_maxx-3] = 0;
    wmove(win, line, (win->_maxx - maxs)/2+1);
    wattron(win, A_REVERSE);
    waddstr(win, s1);
    wattroff(win, A_REVERSE);
}
    

WINDOW *status_box(WINDOW *win, char *s, int line, int create, int no_lines) {
/* Prints a message box with the message 's'. 
 * 
 * Accepts       char s        string to print
 *               int line      line to print string on
 *               create        if TRUE, creates the window, otherwise
 *                             uses window handle in 'win'
 *               no_lines      number of lines of box 
 * Returns       handle of window
*/
    int screen_width;
    int screen_len;
    int width;

    if (!create && (win == NULL))		 /* no window to print on */
      return NULL;
    screen_width = curscr->_maxx;
    screen_len = curscr->_maxy;
    width = min(max(strlen(s)+5, screen_width/2), screen_width - 4);
    if (create) {
	win  = my_newwin(6+no_lines, width, 
		      screen_len/2-(6+no_lines)/2,
		      (screen_width-width)/2);
	my_werase(win, COLOR_STATUS);
	wrefresh(win);
    }
    if (has_colors()) wattron(win, COLOR_PAIR(COLOR_STATUS));
    centre(win, 2+line, s, COLOR_STATUS);
    wrefresh(win);
    return win;
}


void close_statusbox(WINDOW *win) {
    if (win)
      my_delwin(win);
}


void mb_options(WINDOW *mes_box, int mes_box_lines, int type, int res, int bold)
{
/* Prints the options for a message box.
 * 
 * If bold==1, then it's printed bold, otherwise it's inverse */
    
    switch(type) {
     case MB_RETRYABORT:
	if (res) {
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, 3, "RETRY");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 7, "ABORT");
	}
	else {
	    mvwaddstr(mes_box, mes_box_lines- 1, 3, "RETRY");
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 7, "ABORT");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	}
	break;
     case MB_YESNO:
	if (res) {
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, 3, "YES");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 5, "NO");
	}
	else {
	    mvwaddstr(mes_box, mes_box_lines- 1, 3, "YES");
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 5, "NO");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	}
	break;
     case MB_APPENDOVERWRITE:
	if (res) {
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, 3, "APPEND");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 12, "OVERWRITE");
	}
	else {
	    mvwaddstr(mes_box, mes_box_lines- 1, 3, "APPEND");
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 12, "OVERWRITE");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	}
	break;
     case MB_OKCANCEL:
	if (res) {
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, 3, "OK");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 9, "CANCEL");
	}
	else {
	    mvwaddstr(mes_box, mes_box_lines- 1, 3, "OK");
	    wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	    mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx - 9, "CANCEL");
	    wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	}
	break;
     case MB_OK:
	wattron(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	mvwaddstr(mes_box, mes_box_lines-1, mes_box->_maxx/2-1, "OK");
	wattroff(mes_box, ((bold) ? A_BOLD : A_REVERSE));
	break;
    }
    wrefresh(mes_box);
}


int message_box_engine(WINDOW *mes_box, int type) {
/* Prints a message box with the message 's'. The user has the option of
 * answering YES or NO. The box is printed in the middle of the screen
 * 
 * Accepts       WINDOW *win   window to print message on
 *               char s        string to print
 * Returns       1             if user answered yes
 *               0             if user answered no
*/
    int     res=1, c;
    int     mes_box_lines=mes_box->_maxy;

    while (1) {
	mb_options(mes_box, mes_box_lines, type, res, 0);
	c = wgetch(mes_box);
	switch(c) {
	 case 'y':
	 case 'Y':
	    res = 1; break;
	 case 'n':
	 case 'N':
	    res = 0; break;
	 case ' ':
	 case '\t':
	 case KEY_LEFT:
	 case KEY_RIGHT:
	    res = !res; break;
	}
	if ((c == '\n') || (c=='\r')) break;
    }
    mb_options(mes_box, mes_box_lines, type, res, 1);
    my_delwin(mes_box);
    return res;
}

    
int message_box(char *s, int type) {
/* Prints a message box with the message 's'. The user has the option of
 * answering YES or NO. The box is printed in the middle of the screen
 * 
 * Accepts       WINDOW *win   window to print message on
 *               char s        string to print
 * Returns       1             if user answered yes
 *               0             if user answered no
*/
    WINDOW  *mes_box;
    int screen_width=curscr->_maxx;
    int screen_len=curscr->_maxy;
    int w;

    w = min(max(strlen(s)+10, 15), screen_width-4);
    mes_box = my_newwin(7, w, screen_len/2-4, (screen_width-w)/2);
    my_werase(mes_box, COLOR_DIALOG);
    wrefresh(mes_box);
    keypad(mes_box, TRUE);
    centre(mes_box, 2, s, COLOR_DIALOG);
    return message_box_engine(mes_box, type);
}

    
int multi_message_box(char lns[][150], int count, int type) {
/* Prints a message box with multiple lines. 
 * 
 * Each line is a string. Expect count strings. Each string is 150 long
 * 
 * Accepts       type          type of message box
 *               lns           an array of strings
 * Returns       1             if user answered yes
 *               0             if user answered no
*/
    WINDOW  *mes_box;
    int screen_width=curscr->_maxx - curscr->_begx;
    int screen_len=curscr->_maxy - curscr->_begy;
    int c=0, max_len=0;

    while (c<count) {
	max_len = max(max_len, strlen(lns[c]));   /* work out max string length */
	c++;
    }
    mes_box = my_newwin(4+count, max_len+10,
		     screen_len/2-(2+count/2),
		     (screen_width-(max_len+10))/2);
    my_werase(mes_box, COLOR_DIALOG);
    wrefresh(mes_box);
    keypad(mes_box, TRUE);
    c=0;
    while (c<count) {
	centre(mes_box, 1+c, lns[c], COLOR_DIALOG);
	c++;
    }
    return message_box_engine(mes_box, type);
}

    
void clear_sd(select_details *sd) {
    sd->top_entry = 0;
    sd->cur_entry = 0;
    sd->cursor_line = 1;
}


void check_eop(WINDOW *win, select_details *sd, _s32 no_entries, print_screen ps)
{
    char p=0;
    
    if (no_entries > win->_maxy - 2) {
	sd->cursor_line = win->_maxy-2;
	sd->top_entry = no_entries-(win->_maxy-2);
	sd->cur_entry = no_entries-1;
    }
   else {
	sd->top_entry = 0;
	sd->cursor_line = no_entries;
	sd->cur_entry = no_entries-1;
    }
    ps(win, sd->top_entry, &p);	 
}
    

    
void print_scroll_bar(WINDOW *win, _s32 no_entries, select_details *sd,
		      int s_len, int s_width, char *p_scroll)
{
    int c;
    
    if ((sd->top_entry == 0) && (sd->top_entry+s_len-2 >= no_entries)) {
	if (*p_scroll)
	  box(win, ACS_VLINE, ACS_HLINE);
	*p_scroll = 0;
	return;
    }
    (sd->top_entry) ? mvwaddch(win, 2, s_width, ACS_UARROW) : 
                      mvwaddch(win, 2, s_width, ACS_HLINE);
    (sd->top_entry+s_len-2 < no_entries) ? mvwaddch(win, s_len-2, s_width, ACS_DARROW):
                                            mvwaddch(win, s_len-2, s_width, ACS_HLINE);
    for (c=3;c<s_len-2;c++)
      mvwaddch(win,c,s_width,ACS_BLOCK);
    mvwaddch(win, 3+sd->cur_entry*(s_len-5)/no_entries, s_width, ACS_CKBOARD);
    *p_scroll = 1;
}

    
int select_box(WINDOW *win, _s32 *no_entries,  select_details *sd,
	       print_screen ps, print_line pl, tag_entry te, delete_entry de,
	       refresh_screen rs,
	       WINDOW *sel_win, _s32 *sel_no_entries,
	       select_details *sel_sd, print_screen sel_ps,
	       print_screen asterix, select_details *filed,
	       char *select_help, sr_backupset save_backupset,
	       sr_restoreset rest_backupset) 
{
    _s32  c;
    int   screen_len, screen_width, teret;
    char  p_scroll=0, dummy;
    WINDOW *help;
    char  s[100];

    screen_width = win->_maxx;
    screen_len = win->_maxy;
    ps(win, sd->top_entry, &p_scroll); 
    while (1) {
	if (!(*no_entries)) {			 /* deleted all entries */
	    clear_sd(sd);
	    return SELECT_TAB;			 /* move to next window */
	}
	print_scroll_bar(win,*no_entries, sd, screen_len, screen_width, &p_scroll);
	wattron(win, A_REVERSE);		 /* highlight current line */
	pl(win, sd->cur_entry, sd->cursor_line, TRUE); 
	c = wgetch(win);			 /* get key entry */
	wattroff(win, A_REVERSE);
	pl(win, sd->cur_entry, sd->cursor_line, TRUE); 
	switch(c) {
	 case 'B':
	 case 'b':
	    if (save_backupset) {
		save_backupset(0); 
		rs();
		if (sel_ps) sel_ps(sel_win, sel_sd->top_entry, &p_scroll);
	    }
	    break;
	 case 'L':
	 case 'l':
	    if (rest_backupset) {
		rest_backupset();
		rs();
		if (sel_ps) sel_ps(sel_win, sel_sd->top_entry, &p_scroll);
	    }
	    break;
	 case 'q':
	 case 'Q':
	 case 'a':
	 case 'A':
	    if (message_box("Quit (return to main menu)?", MB_YESNO))
	      return SELECT_ABORT;
	    rs();
	    break;
	 case 'f':
	 case 'F':
	    if (message_box("Finished selection?", MB_YESNO))
	      return SELECT_FINISHED;
	    rs();
	    break;
	 case KEY_UP:
	 case KEY_LEFT:
	    sd->cursor_line--; sd->cur_entry--; break;
	 case KEY_DOWN:				 /* plus fall through from above */
	 case KEY_RIGHT:
	    sd->cursor_line++; sd->cur_entry++; break;
	 case '\t':
	    return SELECT_TAB;
	 case '\n':
	 case '\r':
	    return SELECT_ENTER;
	 case 'u':
	 case 'U':
	    if (de) {				 /* only if delete function enabled */
		de(sd, no_entries);
		sel_sd->cur_entry = min(*sel_no_entries, sel_sd->cur_entry);
		sel_ps(sel_win, sel_sd->top_entry, &dummy);
		if (asterix)
		  asterix(sel_win, filed->top_entry, &dummy); 
		rs();
	    }
	    break;
	 case 'S':				 /* only if tag function enabled */
 	 case 's':
	 case ' ':
	    if (te) {
		teret = te(sd->cur_entry);
		pl(win, sd->cur_entry, sd->cursor_line, TRUE); 
		if (teret == 1)			 /* need to position on last line */
		  check_eop(sel_win, sel_sd, *sel_no_entries, sel_ps);
		rs();
		if (sd->cur_entry + 1 < *no_entries) {
		    sd->cursor_line++; 
		    sd->cur_entry++; 
		}
	    }
	    break;	 /* make it go down a line */
	 case KEY_PPAGE:
 	    if (sd->top_entry-(screen_len-4) >= 0) {
		sd->top_entry -= screen_len-4;
		sd->cur_entry -= screen_len-4;
		ps(win, sd->top_entry, &p_scroll); 
		break;
	    }
	 case KEY_HOME:				 /* also fall through from above */
	    sd->top_entry = 0;			 /* if reached TOP*/
	    sd->cur_entry = 0;
	    sd->cursor_line = 1;
	    ps(win, sd->top_entry, &p_scroll);
	    break;
	 case KEY_NPAGE:
	    if (sd->top_entry + screen_len-2 < *no_entries) {
		sd->top_entry += screen_len-2;
		sd->cur_entry += screen_len-2;
		ps(win, sd->top_entry, &p_scroll);
		break;
	    }
	  case KEY_END:				 /* also fall through from above */
	    sd->top_entry = *no_entries  - (screen_len-2);/* if reach EOP */
	    sd->cur_entry = *no_entries-1;
	    sd->cursor_line = screen_len-2;
	    if (sd->cur_entry < sd->cursor_line) {
		sd->cursor_line = sd->cur_entry+1;
		sd->top_entry = 0;
	    }
	    ps(win, sd->top_entry, &p_scroll);
	    break;
	 default:
	    help = my_newwin(21, curscr->_maxx-10,
			  (curscr->_maxy-20)/2,
			  10/2);
	    my_werase(help, COLOR_HELP);
	    box(help, ACS_VLINE, ACS_HLINE);
	    centre(help, 1, "Commands Available", COLOR_HELP);
	    centre(help, 2, "------------------", COLOR_HELP);
	    centre(help, 4, "Up arrow, Left arrow        move cursor up                     ", COLOR_HELP);
	    centre(help, 5, "Down arrow, Right arrow     move cursor down                   ", COLOR_HELP);
	    centre(help, 6, "Page up                     move cursor up a page              ", COLOR_HELP);
	    centre(help, 7, "Page down                   move cursor down a page            ", COLOR_HELP);
	    centre(help, 8, "End                         move cursor to end of screen       ", COLOR_HELP);
	    centre(help, 9, "Home                        move cursor to beginning of screen ", COLOR_HELP);
	    centre(help,10, "TAB                         move to next panel                 ", COLOR_HELP);
	    centre(help,11, "H, h, ?                     print this screen                  ", COLOR_HELP);
	    c=12;
	    if (de)
	     centre(help,c++,"U, u                        unselect                           ", COLOR_HELP);
	    if (te) {
		sprintf(s,  "S, s, space                 %-36s", select_help);
		centre(help,c++,s,COLOR_HELP);
	    }
	    if (save_backupset) {
	     centre(help,c++,"B, b                        Save file set                      ", COLOR_HELP);
	     centre(help,c++,"L, l                        Load file set                      ", COLOR_HELP);
	    }
	    centre(help,c++,"Q, q, A, a                  abort                              ", COLOR_HELP);
	    centre(help,c++,"F, f                        finish selecting and start         ", COLOR_HELP);
	    centre(help,c+1, "Press any key...", COLOR_HELP);
	    wrefresh(help);
	    wgetch(help);
	    my_delwin(help);
	    rs();
	    break;
	}
	if (sd->cursor_line < 1) {	       	 /* went off top of screen */
	    sd->top_entry--;
	    if (sd->top_entry < 0) {		 /* no entries off top */
		sd->top_entry = 0;
		sd->cur_entry++;
	    }
	    ps(win, sd->top_entry, &p_scroll);      
	    sd->cursor_line = 1;
	}
	
	if (sd->cursor_line > screen_len-2) {	 /* went off bottom */
	    sd->top_entry++;
	    if (sd->top_entry + screen_len-2 > *no_entries) {
		sd->top_entry--;
		sd->cur_entry--;
	    }
	    sd->cursor_line = screen_len-2;
	    ps(win, sd->top_entry, &p_scroll);
	}
	if (sd->cur_entry > *no_entries-1) {
	    sd->cur_entry = *no_entries-1;
	    sd->cursor_line = sd->cur_entry - sd->top_entry + 1;
	}
    }
    return 0; /* shouldn't get here */
}


int select_menu(WINDOW *win, char *menu_items[], int *in_op) 
{
/* This function accepts a string of menu options and lets the
   user select one of them

   If the user presses 'Q', or 'q', it simulates the last option on the
   menu
*/

  int  no_items=0, c=0, k;
  int  sl=win->_maxy, tl;
  int  cur_line=0;
  
  while (1) {					 /* work out how many */
    if (menu_items[no_items]==NULL)		 /* menu items there are */
      break;
    no_items++;
  }

  no_items--;
  tl = (sl - 2*no_items) / 2;			 /* work out where top of menu is */
  for  (c=0; c<=no_items; c++) 
    centre(win, tl+2*c, menu_items[c], COLOR_MAIN); 

    cur_line = *in_op;
  while (1) {
    centre_invert(win, tl+2*cur_line, menu_items[cur_line], COLOR_MAIN);
    k = wgetch(win);
    centre(win, tl+2*cur_line, menu_items[cur_line], COLOR_MAIN);
    switch(k) {
     case KEY_LEFT:
     case KEY_UP:
      cur_line--;
      break;
     case KEY_RIGHT:
     case KEY_DOWN:
      cur_line++;
      break;
     case KEY_END:
	cur_line = no_items;
	break;
     case KEY_HOME:
	cur_line = 0;
	break;
     case 'q':					 /* simulate */
     case 'Q':					 /* pressing last option */
	*in_op = cur_line;
      return no_items;
     case '\n':
     case '\r':
	*in_op = cur_line;
      return cur_line;
    }
    if (cur_line > no_items)
      cur_line = 0;
    if (cur_line < 0)
      cur_line = no_items;
  }
  return 0; /* shouldn't get here */
}




char *convert(char *s, _s32 num)
{
/* prints a long number in the form 123,456,789  */
    
    sprintf(s, "%d,%03d,%03d", (int) (num/1000000L), (int) ((num%1000000L)/1000L),
	    (int) ((num%1000000L)%1000L));
    if (!(num /1000000L)) 
	sprintf(s, "%d,%03d", (int) (num/1000L), (int) (num%1000L));
    if (!(num / 1000L)) 			 /* number is < 1000 */
	sprintf(s, "%d", (int) num);
    return s;
}

	
int comp(const void *da1, const void *da2) {
/* sorting the directory */
    struct direntry *d1, *d2;

    d1 = (struct direntry *) da1;
    d2 = (struct direntry *) da2;
    if (S_ISDIR(d1->org_info.st_mode)) 
	if (S_ISDIR(d2->org_info.st_mode))		 /* both directories */
	  return strcmp(d1->entry.d_name, d2->entry.d_name);
	else
	  return -1;				 /* 1 = dir, 2 = file */
    if (S_ISDIR(d2->org_info.st_mode))		 /* 1 = file, 2 = directory */
	return 1;
    return strcmp(d1->entry.d_name, d2->entry.d_name);     /* both files */
}


int read_dir(char *dir_name, char *prefix, modify_filename fn, 
	     struct direntry **den, _s32 *den_count) {
/* Reads the directory pointed to by 'dir' into memory.
 *
 *  Only selects files that are prefixed with 'prefix'.
 * 
 * It assumes that an old directory is still in memory pointed to by
 * directory and it deallocates this. It then reads the directory
 * into memory

   If the function fn != NULL, then, if the file is to be included in the
   directory, this function is called to give the caller the opportunity
   to modify the filename.

   Returns -1 if error
*/
    DIR            *dir_ptr;
    struct direntry *cur_ent;
    struct dirent  *x;
    _s32            count=0, in_count;

    if ((dir_ptr = opendir(dir_name)) == NULL)	{ /* open directory */
	switch(errno) {
	 case EACCES:
	    message_box("No permission", MB_OK);
	    break;
	 default:
	    return do_exit(ERROR_ILLEGAL_DIR);	 /* problem opening dir */
	}
	return 0;
    }
    chdir(dir_name);
    getcwd(dir_name, MAX_FNAME);		 /* real name */	
    *den_count = 0;				 /* count entries in dir */
    while (readdir(dir_ptr)) (*den_count)++; 
    closedir(dir_ptr);
    if (!*den_count) return 1;			 /* empty directory */
    
    *den = my_realloc((void *) *den, sizeof(struct direntry)    /*  */
	     * *den_count);			 /* make memory */
    cur_ent = *den;

    if ((dir_ptr = opendir(dir_name)) == NULL)	 /* open directory */
      do_exit(ERROR_ILLEGAL_DIR);		 /* problem opening dir */

    in_count = 0;
    while (count < *den_count) {		 /* read into memory */
	x = readdir(dir_ptr);
	if (x==NULL)				 /* less dir entries than we  */
	  break;				 /* thought */
	if (lstat(x->d_name, &cur_ent->info) == -1) /* read info into memory */
	  return do_exit(ERROR_GETINFO); 
	if (stat(x->d_name, &cur_ent->org_info) == -1)
	  cur_ent->org_info = cur_ent->info;	 /* can't get info of link - use link data */
	cur_ent->entry = *x; 
	if ( (((strlen(x->d_name) > strlen(prefix)) &&
	        !strncmp(x->d_name, prefix, strlen(prefix))) ||    /* only if prefix matches */
	    (!strcmp(x->d_name, "..")) || 	         /* or if going up dir */
	      (S_ISDIR(cur_ent->org_info.st_mode))) )    /* or if directory */
	  if (strcmp(x->d_name, ".")) { 		 /* don't want the '.' file */
	      if ( (strcmp(x->d_name, "..")) &&	         /* don't let user modify up dir */
		  !S_ISDIR(cur_ent->org_info.st_mode) )  /* or any directories */
		if (fn)			                 /* let caller */
		  fn(cur_ent, dir_name, prefix);         /* modify filename if it wants to */
	      if (!(!strcmp(x->d_name, "..") && ((!*dir_name) || (!strcmp(dir_name, "/"))))) {
		  cur_ent++;	       /* don't include .. directory if in root */
		  in_count++;
	      }
	  }
	count++;
    }
    *den_count = in_count;
    qsort(*den, *den_count, sizeof(struct direntry), comp);   /* sort directory */
    closedir(dir_ptr);
    return 1;
}


void get_file_type(char *s, umode_t mode) {

    s[1] = 0;
    *s = ' ';
    if (S_ISDIR(mode)) 
      *s = 'd';
    if (S_ISLNK(mode))
      *s = 'l';
    if (S_ISBLK(mode))
      *s = 'b';
    if (S_ISCHR(mode))
      *s = 'c';
}


void print_dir_line(WINDOW *win, struct direntry *entry, int line, 
		    char ref, int dir_left_width) {
/* Prints one entry in a directory at line 'line'  
 * Returns the size of the directory entry in bytes */
    

    char       s[NAME_MAX+1], s2[MAX_FNAME];

    get_file_type(s, entry->info.st_mode);
    strcat(s, " "); 
    if (S_ISLNK(entry->info.st_mode))		 /* is it a link */
      strcat(s, "@");
    else
      if (S_ISDIR(entry->info.st_mode))		 /* is it a directory */
      strcat(s, "/");
    else
      if (!S_ISREG(entry->info.st_mode))
	  strcat(s, "+");
    if (S_ISREG(entry->info.st_mode))		 /* only print file size for reg files */
	convert(s2, entry->info.st_size);
    else
      *s2 = 0;
    sprintf(s, "%s%-*s", s, dir_left_width-18, entry->entry.d_name);
    s[dir_left_width-18] = 0; sprintf(s, "%s %12s", s, s2);
    mvwaddstr(win, line+1, 2, s);
    wattroff(win, A_BOLD);
    if (ref)
      wrefresh(win);
}


void sb_print_dir_line(WINDOW *win, _s32 ent_num, int line, char ref)
{
    print_dir_line(win, sb_directory+ent_num, line, ref, sb_dir_left_width);
}
    

void print_dir(WINDOW *win, _s32 start, char *p_scroll) {
/* This function prints the current directory into the left hand
 * window. It starts printing the directory at entry 'start'.
 * 
 * cur_dir is the name of the current directory, which is printed
 * at the top line of the screen
 * 
 * Format of an entry is:
 *   *name rwxrwxrwx 99999K
 * 
 * where * = ' ', '/' or '@' depending on file type 
*/
    int        cur_line=1;
    _s32       cur_ent;
    _s32       dsize=0;
    char       s[MAX_FNAME];
    struct direntry *entry;
    
    my_werase(win, COLOR_DIRECTORY);
    cur_ent=start;
    while (cur_line < dir_screen_ylen-2) {
	sb_print_dir_line(win, cur_ent, cur_line, FALSE);/* print line */
	cur_line++;				 /* increment */
	cur_ent++;
	if (start+cur_line > sb_directory_count) /* check that not at end of dir */
	    break;
    }
    *p_scroll = 0;
    print_scroll_bar(win, sb_directory_count, &sb_dir_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wattron(win, A_UNDERLINE); 
    entry=sb_directory;				 /* calculate size of directory */
    for (cur_line=1; cur_line<=sb_directory_count;cur_line++) {
	if (S_ISREG(entry->info.st_mode))	
	  dsize += entry->info.st_size;		 /* only reg files have a size */
	entry++;
    }
    convert(s, dsize);				 /* print dir size at right */
    mvwaddstr(win, 1, win->_maxx-strlen(s)-1, s);
    if (strlen(global_cur_dir) > win->_maxx-strlen(s)-2) {
	strcpy(s, &global_cur_dir[strlen(global_cur_dir)-(win->_maxx-strlen(s)-1)+2]);
	s[0] = '~';
    }
    else
      strcpy(s, global_cur_dir);
    mvwaddstr(win, 1, 1, s);
    wattroff(win, A_UNDERLINE);
}

void dir_paint(void)
{
    touchwin(dir_win); wrefresh(dir_win);
}

void set_correct_sd(char *s, select_details *sd, _s32 dir_count, 
		    struct direntry *dir)
{
/* Sets the default entry as 's' in directory */
    
    _s32 c=0;
    
    while (c < dir_count)
      if (!strcmp(s, (dir+c)->entry.d_name))
	break;
      else
	c++;
    if (c < dir_count) {		 /* found it */
	sd->cur_entry = c;
	sd->top_entry = c-5;			 /* give it 5 lines */
	sd->cursor_line = 6;
	if (sd->top_entry < 0) {		 /* not five entries in directory */
	    sd->cursor_line += sd->top_entry;
	    sd->top_entry = 0;
	}
    }
}


void find_correct_sd(char *old_dir, select_details *sd, _s32 dir_count,
		     struct direntry *dir)
{
/* Used when going up a directory. Finds the entry in the current directory
   where the old directory was.
*/
   
    char *s;

    s = &old_dir[strlen(global_cur_dir)];
    while (*s == '/') s++;
    set_correct_sd(s, sd, dir_count, dir);
}
	  
    
    
int select_file(char *start_dir, char *prefix, char *deflt, modify_filename fn,
		char change_dir)
{
/* This routine gets a file from the user. The start directory is 'start_dir'
   and only files with a prefix of 'prefix' are included

   The filename selected is returned in deflt.
 * 
 * modify_filename is the name of the function that takes the filename
 * and processes it to give the string that should be printed on the 
 * screen
 * 
 * if change_dir is TRUE, then the user is allowed to change dirs
 *                  FALSE, not allowed to

   Returns -1 if error
*/

    
    int   ret;
    char  p=0;
    char  old_dir[MAX_FNAME], cur_dir[MAX_FNAME];

    getcwd(cur_dir, sizeof(cur_dir));		 /* preserve existing dir */
    strcpy(dir_cur_dir, start_dir);		 /* don't alter start_dir */
    global_cur_dir = dir_cur_dir;
    dir_win = newwin(win_main->_maxy+1, win_main->_maxx, /* make window for directory */
		     1, 0);
    my_werase(dir_win, COLOR_DIRECTORY);
    keypad(dir_win, TRUE);
    dir_screen_ylen = dir_win->_maxy+1;
    sb_dir_left_width = dir_win->_maxx;
    
    sb_directory_count = 0;
    if (read_dir(dir_cur_dir, prefix, fn,
		 &sb_directory, &sb_directory_count) == -1)
      return -1;				 /* read in directory */
    clear_sd(&sb_dir_sd);
    set_correct_sd(deflt, &sb_dir_sd, sb_directory_count,
		   sb_directory);
    

    p=-0;
    print_dir(dir_win, sb_dir_sd.top_entry, &p);	 /* print directory */

    while (1) {
	ret = select_box(dir_win, &sb_directory_count, &sb_dir_sd,
			 print_dir, sb_print_dir_line, NULL, 
			 NULL, dir_paint,
			 NULL, NULL, NULL,
			 NULL, NULL, NULL,
			 "", NULL, NULL);
	switch(ret) {
	 case SELECT_ABORT:
	    delwin(dir_win);
	    chdir(cur_dir);
	    return 0;
	 case SELECT_FINISHED:
	    delwin(dir_win);
	    strcpy(deflt, (sb_directory+sb_dir_sd.cur_entry)->entry.d_name); 
	    chdir(cur_dir);
	    return 1;
	 case SELECT_ENTER:
	    if (S_ISDIR((sb_dir_sd.cur_entry+sb_directory)->org_info.st_mode)) {
		if (change_dir) {
		    *old_dir = 0;
		    if (!strcmp((sb_directory+sb_dir_sd.cur_entry)->entry.d_name, ".."))
		      strcpy(old_dir, dir_cur_dir);
		    strcat(dir_cur_dir, "/");
		    strcat(dir_cur_dir, (sb_directory+sb_dir_sd.cur_entry)->entry.d_name);
		    ret = read_dir(dir_cur_dir, prefix, fn,
				   &sb_directory, &sb_directory_count);
		    if (ret == -1) {
			chdir(cur_dir);
			return -1;
		    }
		    if (ret == 0)
		      getcwd(dir_cur_dir, MAX_FNAME);
		    else
		      clear_sd(&sb_dir_sd);
		    if (old_dir)
		      find_correct_sd(old_dir,&sb_dir_sd, 
				      sb_directory_count, sb_directory);
		    p=0;
		    print_dir(dir_win, sb_dir_sd.top_entry, &p);
		}
		else 
		  message_box("Must stay in current directory", MB_OK);
	    }
	    else {				 /* if entered on non-directory */
		delwin(dir_win);		 /* assume selecting */
		strcpy(deflt, (sb_directory+sb_dir_sd.cur_entry)->entry.d_name); 
		chdir(cur_dir);
		return 1;
	    }
	    break;
	 default:
	    break;
	}
    }
    return 0;  /* never gets here */
}



int retryabort(char *s)
{
/* Prints an error message and then offers user the option
 * of retrying or aborting
 * 
 * Returns 0 if retry,
 *        -1 if abort.
*/
    char s1[MAX_FNAME];
    
    strcpy(s1, strerror(errno));
    strcat(s1, " while ");
    strcat(s1, s);
    return (message_box(s1, MB_RETRYABORT) == 1) ? 0 : -1;
}
