/*
 * screen.c
 *
 * Written By Matthew Green, based on portions of window.c
 * by Michael Sandrof.
 *
 * Copyright(c) 1993, 1994.
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#ifdef HAVE_SYS_UN_H
# include <sys/un.h>
#endif /* HAVE_SYS_UN_H */

#include "screen.h"
#include "window.h"
#include "output.h"
#include "vars.h"
#include "server.h"
#include "list.h"
#include "ircterm.h"
#include "names.h"
#include "ircaux.h"
#include "input.h"
#include "log.h"
#include "hook.h"
#include "dcc.h"
#include "exec.h"
#include "newio.h"
#include "misc.h"

#ifdef WINNT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif WINNT

	Window	*to_window;
	Screen	*current_screen;
	Screen	*main_screen;
	Screen	*last_input_screen;

extern	int	in_help;	/* Used to suppress holding of help text */
extern	char	*redirect_nick;
	int	extended_handled = 0;
extern	int	in_add_to_tcl;
	
/* Our list of screens */
Screen	*screen_list = NULL;

static	void	display_lastlog_lines _((int, int, Window *));
static	Screen	* create_new_screen _((void));
static	char	** split_up_line _((char *));
static	void	scrollback_backwards_lines _((int));
static	void	scrollback_forwards_lines _((int));
static	char	display_highlight _((int));
static	char	display_bold _((int));
static	void	add_to_window _((Window *, char *));
static	u_int	create_refnum _((void));
static	char	*next_line_back _((Window *));

extern	int	vt100_decode _((char));
extern	char	*stripansicodes _((char *));
	void	add_to_tcl	_((Window *, char *));


/* Checks if ansi string only sets colors/attributes */
/* Once again borrowed from Flier */
/* Checks if ansi string only sets colors/attributes
 * ^[[m won't work anymore !!!!
 */
void FixColorAnsi(str)
char *str;
{
    register char *tmpstr;
    register char *tmpstr1=NULL;
    int  what=0;
    int  numbers=0;
    int	 val = 0;

	tmpstr=str;
	while (*tmpstr) 
	{
		if ((*tmpstr>='0' && *tmpstr<='9')) 
		{
			numbers = 1;
			val = val * 10 + (*tmpstr - '0');
		}
		else if (*tmpstr==';')
			numbers = 1, val = 0;
		else if (!(*tmpstr=='m' || *tmpstr=='C')) 
			numbers = val = 0;
		if (*tmpstr==0x1B) 
		{
			if (what && tmpstr1) 
				*tmpstr1+=64;
			what=1;
			tmpstr1=tmpstr;
		}
		else if (*tmpstr==0x18 || *tmpstr==0x0E) 
			*tmpstr+=64;
		if (what && (*tmpstr=='m' || *tmpstr=='C')) 
		{
			if (!numbers || val == 12) 
			{
				*tmpstr1+=64;
				tmpstr1=tmpstr;
			}
			what=0;
			numbers = val = 0;
		}
		else if (what && *tmpstr=='(') 
			what=2;
		else if (what==2 && *tmpstr=='U') 
			what=0;
		tmpstr++;
	}
	if (what && tmpstr1 && *tmpstr1) 
		*tmpstr1+=64;
/*
	if ((tmpstr = strstr(str, "[11m")) || (tmpstr = strstr(str, "[12m")))
		*tmpstr = *tmpstr+=64;
*/
}

/* how many ansi escape chars do we have in string?
   if len -1 count full string, otherwise, count len chars */

#ifdef __STDC__
int count_ansi(char *str, int len)
#else
int count_ansi(str, len)
char *str;
int len;
#endif
{
        register int count = 0;
        int x = 0;
        register char *ptr;
	FixColorAnsi(str);
        if (len == -1) {
                for (ptr = str; *ptr; ptr++)
                        if (vt100_decode(*ptr))
                                count++;
        }
        else {
                for (ptr = str; *ptr && x < len; ptr++, x++)
                        if (vt100_decode(*ptr))
                                count++;
        }
        return count;
}

/*
 * create_new_screen creates a new screen structure. with the help of
 * this structure we maintain ircII windows that cross screen window
 * boundaries.
 */
static	Screen	*
create_new_screen()
{
	Screen	*new = NULL,
		**list;
	static	int	refnumber = 0;
	int i;
	
	for (list = &screen_list; *list; list = &((*list)->next))
	{
		if (!(*list)->alive)
		{
			new = *list;
			break;
		}
	}
	if (!new)
	{
		new = (Screen *) new_malloc(sizeof(Screen));
		bzero(new, sizeof(Screen));
		new->screennum = ++refnumber;
		new->next = screen_list;
		if (screen_list)
			screen_list->prev = new;
		screen_list = new;
	}
	new->last_window_refnum = 1;
	new->window_list = NULL;
	new->window_list_end = NULL;
	new->cursor_window = NULL;
	new->current_window = NULL;
	new->visible_windows = 0;
	new->window_stack = NULL;
	for (i = 0; i <= 9; i++)
		new->meta_hit[i] = 0;                   

	new->quote_hit = new->digraph_hit = new->inside_menu = 0;
	new->buffer_pos = new->buffer_min_pos = 0;
	new->input_buffer[0] = '\0';
	new->fdout = 1;
	new->fpout = stdout;
	new->fdin = 0;
	if (use_input)
		FD_SET(0, &readables);
	new->fpin = stdin;
#ifdef WINNT
	new->hStdin = GetStdHandle(STD_INPUT_HANDLE);
	new->hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
#endif WINNT
	new->alive = 1;
	new->promptlist = NULL;
	new->redirect_name = NULL;
	new->redirect_token = NULL;
	new->tty_name = NULL;
	new->li = 24;
	new->co = 79;
	new->redirect_server = -1;
	last_input_screen = new;
	return new;
}

/* 
 * add_wait_prompt:  Given a prompt string, a function to call when
 * the prompt is entered.. some other data to pass to the function,
 * and the type of prompt..  either for a line, or a key, we add 
 * this to the prompt_list for the current screen..  and set the
 * input prompt accordingly.
 */
void
add_wait_prompt(prompt, func, data, type)
	char	*prompt;
	void	(*func) _((char *, char *));
	char	*data;
	int	type;
{
	WaitPrompt **AddLoc,
		   *New;

	New = (WaitPrompt *) new_malloc(sizeof(WaitPrompt));
	New->prompt = m_strdup(prompt);
	New->data = m_strdup(data);
	New->type = type;
	New->func = func;
	New->next = NULL;
	for (AddLoc = &current_screen->promptlist; *AddLoc;
			AddLoc = &(*AddLoc)->next);
	*AddLoc = New;
	if (AddLoc == &current_screen->promptlist)
		change_input_prompt(1);
}

#ifdef __STDC__
void set_current_screen(Screen *screen)
#else
void set_current_screen(screen)
	Screen	*screen;
#endif
{
	current_screen = (screen && screen->alive) ? screen : screen_list; 
}

/*
 * window_redirect: Setting who to non null will cause IRCII to echo all
 * commands and output from that server (including output generated by IRCII)
 * to who.  Setting who to null disables this 
 */
void
window_redirect(who, server)
	char	*who;
	int	server;
{
	char	buf[BIG_BUFFER_SIZE];

	if (who)
		sprintf(buf, "%04d%s", server, who);
	else
		sprintf(buf, "%04d#LAME", server);
	malloc_strcpy(&current_screen->redirect_token, buf);
	malloc_strcpy(&current_screen->redirect_name, who);
	current_screen->redirect_server = server;
}

int
check_screen_redirect(nick)
	char	*nick;
{
	Screen	*screen,
		*tmp_screen;
	
	for (screen = screen_list; screen; screen = screen->next)
	{
		if (!screen->redirect_token)
			continue;
		if (!strncmp(nick, screen->redirect_token, strlen(screen->redirect_token)))
		{
			tmp_screen = current_screen;
			current_screen = screen;
			window_redirect(NULL, from_server);
			current_screen = tmp_screen;
			return 1;
		}
	}
	return 0;
}

#if 0
static	int is_main_screen _((Screen *screen))
{
	return 1;
}
#endif

/*
 * scroll_window: Given a pointer to a window, this determines if that window
 * should be scrolled, or the cursor moved to the top of the screen again, or
 * if it should be left alone. 
 */
void scroll_window(window)
	Window	*window;
{
	if (dumb)
		return;
	if (window->cursor == window->display_size)
	{
		if (window->scroll)
		{
			int	scroll,
			i;

			if ((scroll = get_int_var(SCROLL_LINES_VAR)) <= 0)
				scroll = 1;

			for (i = 0; i < scroll; i++)
			{
				new_free (&window->top_of_display->line);
				window->top_of_display =
					window->top_of_display->next;
			}
			if (window->visible)
			{
				term_scroll(window->top+window->menu.lines,
					window->top + window->menu.lines+
					window->cursor - 1, scroll);
				window->cursor -= scroll;
				term_move_cursor(0, window->cursor + window->top + window->menu.lines/* + window->status_split*/);
			}
			else
				window->cursor -= scroll;
		}
		else
		{
			window->cursor = 0;
			if (window->visible)
				term_move_cursor(0, window->top + window->menu.lines/*+window->status_split*/);
		}
	}
	else if (window->visible && current_screen->cursor_window == window)
	{
		term_cr();
		term_newline();
	}
	if (window->visible && current_screen->cursor_window)
		term_clear_to_eol();
}

/* display_highlight: turns off and on the display highlight.  */
static	char
display_highlight(flag)
	int	flag;
{
	static	int	highlight = OFF;

	if (flag == highlight)
		return (flag);
	switch (flag)
	{
		case ON:
		{
			highlight = ON;
			if (get_int_var(INVERSE_VIDEO_VAR))
				term_standout_on();
			return (OFF);
		}
		case OFF:
		{
			highlight = OFF;
			if (get_int_var(INVERSE_VIDEO_VAR))
				term_standout_off();
			return (ON);
		}
		case TOGGLE:
		{
			if (highlight == ON)
			{
				highlight = OFF;
				if (get_int_var(INVERSE_VIDEO_VAR))
					term_standout_off();
				return (ON);
			}
			else
			{
				highlight = ON;
				if (get_int_var(INVERSE_VIDEO_VAR))
					term_standout_on();
				return (OFF);
			}
		}
	}
	return flag;
}
 
/* display_bold: turns off and on the display bolding.  */
static	char
display_bold(flag)
	int	flag;
{
	static	int	bold = OFF;

	if (flag == bold)
		return (flag);
	switch (flag)
	{
	case ON:
		bold = ON;
		if (get_int_var(BOLD_VIDEO_VAR))
			term_bold_on();
		return (OFF);
	case OFF:
		bold = OFF;
		if (get_int_var(BOLD_VIDEO_VAR))
			term_bold_off();
		return (ON);
	case TOGGLE:
		if (bold == ON)
		{
			bold = OFF;
			if (get_int_var(BOLD_VIDEO_VAR))
				term_bold_off();
			return (ON);
		}
		else
		{
			bold = ON;
			if (get_int_var(BOLD_VIDEO_VAR))
				term_bold_on();
			return (OFF);
		}
	}
	return OFF;
}

/*
 * output_line prints the given string at the current screen position,
 * performing adjustments for ^_, ^B, ^V, and ^O
 */
int
output_line(str, result, startpos)
	char	*str;
	char	**result;
	int	startpos;
{
	static	int	high = OFF, bold = OFF;
	int	rev_tog, und_tog, bld_tog, all_off;
	char	*ptr;
	int	len;
	int	written = 0;
	char	c;
	char	*original;
	int 	ansi_count = 0;

	original = str;
	ptr = str;
	display_highlight(high);
	display_bold(bold);
	/* do processing on the string, handle inverse and bells */
	while (*ptr)
	{
		switch (*ptr)
		{
		case REV_TOG:
		case UND_TOG:
		case BOLD_TOG:
		case ALL_OFF:
		{
			len = ptr - str;
			written += len;
			ansi_count = count_ansi(str, ptr - str);
			written -= ansi_count;
			if (startpos)
			{
				if (ptr - original > startpos)
				{
					str += len - (ptr - original - startpos);
					len = ptr - original - startpos;
					startpos = 0;
				}
			}
			if (written > CO)
				len = len - (written - CO);
			if (!startpos)
				term_puts(str, len);
			ansi_count = 0;
			rev_tog = und_tog = bld_tog = all_off = 0;
			do
			{
				switch(*ptr)
				{
					case REV_TOG:
						rev_tog = 1 - rev_tog;
						break;
					case UND_TOG:
						und_tog = 1 - und_tog;
						break;
					case BOLD_TOG:
						bld_tog = 1 - bld_tog;
						break;
					case ALL_OFF:
						all_off = 1;
						und_tog = rev_tog = bld_tog = 0;
						break;
				}
			} while ((ptr[1] == REV_TOG || ptr[1] == UND_TOG ||
			    ptr[1] == BOLD_TOG || ptr[1] == ALL_OFF) && ptr++);

			if (all_off)
			{
				if (!underline)
				{
					term_underline_off();
					underline = 1;
				}
				display_highlight(OFF);
				display_bold(OFF);
				high = 0;
				bold = 0;
			}
			if (und_tog && get_int_var(UNDERLINE_VIDEO_VAR))
			{
				if ((underline = 1 - underline) != 0)
					term_underline_off();
				else
					term_underline_on();
			}
			if (rev_tog)
			{
				high = display_highlight(TOGGLE);
				high = 1 - high;
			}
			if (bld_tog)
			{
				bold = display_bold(TOGGLE);
				bold = 1 - bold;
			}
			str = ++ptr;
			break;
		}
		case '\007':
		/*
		 * same as above, except after we display everything
		 * so far, we beep the terminal 
		 */
			c = *ptr;
			*ptr = '\0';
			len = strlen(str);
			ansi_count = count_ansi(str, -1);
			written += len;
			written -= ansi_count;
			if (startpos)
			{
				if (ptr - original > startpos)
				{
					str += len - (ptr - original - startpos);
					len = ptr - original - startpos;
					startpos = 0;
				}
			}
			if (written > CO)
				len = len - (written - CO);
			if (!startpos)
				term_puts(str, len);
			term_beep();
			*ptr = c;
			str = ++ptr;
			ansi_count = 0;
			break;
		default:
			ptr++;
			break;
		}
	}
	if (result)
		*result = str;
	return written;
}

/*
 * rite: this routine displays a line to the screen adding bold facing when
 * specified by ^Bs, etc.  It also does handles scrolling and paging, if
 * SCROLL is on, and HOLD_MODE is on, etc.  This routine assumes that str
 * already fits on one screen line.  If show is true, str is displayed
 * regardless of the hold mode state.  If redraw is true, it is assumed we a
 * redrawing the screen from the display_ip list, and we should not add what
 * we are displaying back to the display_ip list again. 
 *
 * Note that rite sets display_highlight() to what it was at then end of the
 * last rite().  Also, before returning, it sets display_highlight() to OFF.
 * This way, between susequent rites(), you can be assured that the state of
 * bold face will remain the same and the it won't interfere with anything
 * else (i.e. status line, input line). 
 */
int
rite(window, str, show, redraw, backscroll, logged)
	Window	*window;
	char	*str;
	int	show,
		redraw,
		backscroll,
		logged;
{
	static	int	high = OFF;
	int	written = 0,
		ansi_count = 0,
		len;
	Screen	*old_current_screen = current_screen;

	if (!backscroll && window->scrolled_lines)
		window->new_scrolled_lines++;
	if (window->hold_mode && window->hold_on_next_rite && !redraw && !backscroll)
	{
		/* stops output from going to the window */
		window->hold_on_next_rite = 0;
		hold_mode(window, ON, 1);
		if (show)
			return (1);
	}
	/*
	 * Don't need to worry about the current_screen if the window isn't
	 * visible, as hidden windows aren't attached to a screen anyway
	 */
	if (window->visible)
	{
		old_current_screen = current_screen;
		set_current_screen(window->screen);
	}
	if (!show && (hold_output(window) || hold_queue(window)) && !in_help && !redraw && !backscroll)
		/* sends window output to the hold list for that window */
		add_to_hold_list(window, str, logged);
	else
	{
		if (!redraw && !backscroll)
		{
		/*
		 * This isn't a screen refresh, so add the line to the display
		 * list for the window 
		 */
			if (window->scroll)
				scroll_window(window);
			malloc_strcpy(&(window->display_ip->line), str);
			window->display_ip->linetype = logged;
			window->display_ip = window->display_ip->next;
			if (!window->scroll)
				new_free(&window->display_ip->line);
		}

		if (!window->visible && window->beep_always && index(str, '\007'))
		{
			Window *old_to_window;
			term_beep();
			old_to_window = to_window;
			to_window = curr_scr_win;
			say("Beep in window %d", window->refnum);
			to_window = old_to_window;
		}			
		if (window->visible)
		{
			/* make sure the cursor is in the appropriate window */
			if (current_screen->cursor_window != window &&
					!redraw && !backscroll)
			{
				current_screen->cursor_window = window;
				term_move_cursor(0, window->cursor +
					window->top + window->menu.lines/*+ window->status_split*/);
			}
			written = output_line(str, &str, 0);
			len = strlen(str);
			ansi_count = count_ansi(str, -1);
			written += len;
			written -= ansi_count;
			if (written > CO)
				len = len - (written - CO);
			if (len > 0)
				term_puts(str, len);
			term_clear_to_eol();
		}
		else if (!(window->miscflags & WINDOW_NOTIFIED))
		{
			if (who_level & window->notify_level)
			{
				window->miscflags |= WINDOW_NOTIFIED;
				if (window->miscflags & WINDOW_NOTIFY)
				{
					Window	*old_to_window;
					int	lastlog_level;

					lastlog_level =
						set_lastlog_msg_level(LOG_CRAP);
					old_to_window = to_window;
					to_window = curr_scr_win;
					say("Activity in window %d",window->refnum);
					to_window = old_to_window;
					set_lastlog_msg_level(lastlog_level);
				}
				update_all_status(curr_scr_win, NULL, 0);
			}
		}

		if (!redraw && !backscroll)
		{
			window->cursor++;
			window->line_cnt++;
			if (window->scroll)
			{
				if (window->line_cnt >= window->display_size)
				{
					window->hold_on_next_rite = 1;
					window->line_cnt = 0;
				}
			}
			else
			{
				scroll_window(window);
				if (window->cursor == (window->display_size -1))
					window->hold_on_next_rite = 1;
			}

			if (window->visible)
				high = display_highlight(OFF);
		}
		else if (window->visible)
		{
			high = display_highlight(OFF);
			term_cr();
			term_newline();
		}
	}
	if (window->visible)
		set_current_screen(old_current_screen);
	return (0);
}

/*
 * cursor_not_in_display: This forces the cursor out of the display by
 * setting the cursor window to null.  This doesn't actually change the
 * physical position of the cursor, but it will force rite() to reset the
 * cursor upon its next call 
 */
void cursor_not_in_display _((void))
{
	current_screen->cursor_window = NULL;
}

/*
 * cursor_in_display: this forces the cursor_window to be the
 * current_screen->current_window. 
 * It is actually only used in hold.c to trick the system into thinking the
 * cursor is in a window, thus letting the input updating routines move the
 * cursor down to the input line.  Dumb dumb dumb 
 */
void cursor_in_display _((void))
{
	current_screen->cursor_window = curr_scr_win;
}

/*
 * is_cursor_in_display: returns true if the cursor is in one of the windows
 * (cursor_window is not null), false otherwise 
 */
#ifdef __STDC__
int is_cursor_in_display(Screen *screen)
#else
int is_cursor_in_display(screen)
	Screen *screen;
#endif
{
	return ((screen ? screen : current_screen)->cursor_window ? 1 : 0);
}

void
redraw_resized(window, Info, AnchorTop)
	Window	*window;
	ShrinkInfo Info;
	int	AnchorTop;
{
	if (!AnchorTop)
	{
		if (Info.bottom < 0)
			term_scroll(window->top+window->menu.lines/*+window->status_split*/+Info.bottom,
				window->top + window->menu.lines/* + window->status_split*/ +
				window->display_size - 1, Info.bottom);
		else if (Info.bottom)
			term_scroll(window->top+window->menu.lines/* + window->status_split*/,
				window->top + window->menu.lines/* + window->status_split*/ +
				window->display_size -1, Info.bottom);
	}
}

/*
 * resize_display: After determining that the screen has changed sizes, this
 * resizes all the internal stuff.  If the screen grew, this will add extra
 * empty display entries to the end of the display list.  If the screen
 * shrank, this will remove entries from the end of the display list.  By
 * doing this, we try to maintain as much of the display as possible. 
 *
 * This has now been improved so that it returns enough information for
 * redraw_resized to redisplay the contents of the window without having
 * to redraw too much.
 */
ShrinkInfo
resize_display(window)
	Window	*window;
{
	int	cnt,
		i;
	Display *tmp, *pre_ip;
	int	Wrapped = 0;
	ShrinkInfo Result;

	Result.top = Result.bottom = 0;
	Result.position = window->cursor;
	if (dumb)
		return Result;

	if (!window->top_of_display)
	{
		window->top_of_display = (Display *)new_malloc(sizeof(Display));
		window->top_of_display->line = NULL;
		window->top_of_display->linetype = LT_UNLOGGED;
		window->top_of_display->next = window->top_of_display;
		window->display_ip = window->top_of_display;
		window->old_size = 1;
	}
	/* cnt = size - window->display_size; */
	cnt = window->display_size - window->old_size;
	if (cnt > 0)
	{
		Display *new = NULL;

	/*
	 * screen got bigger: got to last display entry and link in new
	 * entries 
	 */
		for (tmp = window->top_of_display, i = 0; i < (window->old_size - 1); i++, tmp = tmp->next)
			;
		for (i = 0; i < cnt; i++)
		{
			new = (Display *) new_malloc(sizeof(Display));
			new->line = NULL;
			new->linetype = LT_UNLOGGED;
			new->next = tmp->next;
			tmp->next = new;
		}
		if (window->display_ip == window->top_of_display &&
		    window->top_of_display->line)
			window->display_ip = new;
		Result.top = 0;
		Result.bottom = cnt;
		Result.position = 0;
	}
	else if (cnt < 0)
	{
		Display *ptr;

	/*
	 * screen shrank: find last display entry we want to keep, and remove
	 * all after that point 
	 */
		cnt = -cnt;
		for (pre_ip = window->top_of_display;
		    pre_ip->next != window->display_ip;
		    pre_ip = pre_ip->next)
		        ;

		for (tmp = pre_ip->next, i =0; i < cnt; i++, tmp = ptr)
		{
			ptr = tmp->next;
			if (tmp == window->top_of_display)
			{
				if (tmp->line)
					Wrapped = 1;
				window->top_of_display = ptr;
			}
			if (Wrapped)
				Result.top--;
			else
				Result.bottom--;
			new_free(&(tmp->line));
			new_free((char **)&tmp);
		}
		window->display_ip = pre_ip->next = tmp;
		window->cursor += Result.top;
		if (!window->scroll)
		{
			if (window->cursor == window->display_size)
				window->cursor = 0;
			new_free(&window->display_ip->line);
		}
	}
	window->update |= REDRAW_DISPLAY_FULL | REDRAW_STATUS;
	window->old_size = window->display_size;
	return Result;
}

/*
 * recalculate_windows: this is called when the terminal size changes (as
 * when an xterm window size is changed).  It recalculates the sized and
 * positions of all the windows.  Currently, all windows are rebalanced and
 * window size proportionality is lost 
 */
void recalculate_windows _((void))
{
	int	base_size,
	size,
	top,
	extra;
	Window	*tmp = current_screen->window_list;
	
	if (dumb)
		return;

	base_size = ((LI - 1) / current_screen->visible_windows) - 1;
	extra = (LI - 1) - ((base_size + 1)*current_screen->visible_windows);
	top = 0;
	for (tmp = current_screen->window_list; tmp; tmp = tmp->next)
	{
		tmp->update |= REDRAW_DISPLAY_FULL | REDRAW_STATUS;
		if (extra)
		{
			extra--;
			size = base_size + 1;
		}
		else
			size = base_size;
		tmp->display_size = size - tmp->menu.lines - tmp->double_status;

		if (tmp->display_size<=0)
			tmp->display_size = 1;
		tmp->top = top;
		tmp->bottom = top + size - tmp->double_status;
		top += size + 1;
	}
}

/*
 * clear_window: This clears the display list for the given window, or
 * current window if null is given.  
 */
void
clear_window(window)
	Window	*window;
{
	int	i,
		cnt;

	if (dumb)
		return;
	if (window == NULL)
		window = curr_scr_win;
	erase_display(window);
	term_move_cursor(0, window->top + window->menu.lines/* + window->status_split*/);
	cnt = window->bottom - window->top - window->menu.lines;
	for (i = 0; i < cnt; i++)
	{
		term_clear_to_eol();
		term_newline();
	}
	term_flush();
}

/* clear_all_windows: This clears all *visible* windows */
void
clear_all_windows(unhold)
	int	unhold;
{
	Window	*tmp;

	for (tmp = current_screen->window_list; tmp; tmp = tmp->next)
	{
		if (unhold)
			hold_mode(tmp, OFF, 1);
		clear_window(tmp);
	}
}

/*
 * redraw_window: This redraws the display list for the given window. Some
 * special considerations are made if you are just redrawing one window as
 * opposed to using this routine to redraw the whole screen one window at a
 * time 
 *
 * A negative value in just_one indicates not all of the window needs to
 * be redrawn.
 */
void
redraw_window(window, just_one)
	Window	*window;
	int	just_one;
{
	Display *tmp;
	int	i;
	int	StartPoint;
	int	yScr;

	if (dumb || !window->visible)
		return;
	window = window ? window : curr_scr_win;

	if (just_one < 0)
	{
	/* This part of the window is scrolling into view */
		StartPoint = -just_one;
		just_one = 0;
	}
	else
	{
		StartPoint = 0;
		if (window->scrolled_lines)
			display_lastlog_lines(window->scrolled_lines-window->display_size + 1, window->scrolled_lines, window);
	}
	if (window->scrolled_lines + StartPoint < window->display_size)
		yScr = window->scrolled_lines + StartPoint;
	else
		yScr = 0;
	term_move_cursor(0, window->top+window->menu.lines+yScr/*+window->status_split*/);

	for (tmp = window->top_of_display, i = 0; i < window->display_size-window->scrolled_lines; i++, tmp = tmp->next)
	{
		if (i < StartPoint)
			continue;
		if (tmp->line)
			rite(window, tmp->line, 1, 1, 0, 0);
		else
		{
			if (just_one)
				term_clear_to_eol();
			term_newline();
		}
	}
	term_flush();
}

/*
 * recalculate_window_positions: This runs through the window list and
 * re-adjusts the top and bottom fields of the windows according to their
 * current positions in the window list.  This doesn't change any sizes of
 * the windows 
 */
void
recalculate_window_positions()
{
	Window	*tmp = current_screen->window_list;
	int	top;

	top = 0;
	for (tmp = current_screen->window_list; tmp; tmp = tmp->next)
	{
		tmp->update |= REDRAW_DISPLAY_FULL | REDRAW_STATUS;
		tmp->top = top;
		tmp->bottom = top + tmp->display_size + tmp->menu.lines;
		top += tmp->display_size + tmp->menu.lines + 1 + tmp->double_status;
	}
}

/*
 * redraw_all_windows: This basically clears and redraws the entire display
 * portion of the screen.  All windows and status lines are draws.  This does
 * nothing for the input line of the screen.  Only visible windows are drawn 
 */
void redraw_all_windows(void)
{
	Window	*tmp;

	if (dumb)
		return;
	for (tmp = current_screen->window_list; tmp; tmp = tmp->next)
		tmp->update = REDRAW_STATUS | REDRAW_DISPLAY_FAST;
}

#define	MAXIMUM_SPLITS	40
static	char	**
split_up_line(str)
	char	*str;
{
	static	char	*output[MAXIMUM_SPLITS] =
	{ 
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
#if 0
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL
#endif
	};
	char	buffer[ 2 * BIG_BUFFER_SIZE + 1];
	unsigned char *ptr;
	char	*cont_ptr,
		*cont = NULL,
		*temp = NULL,
		c;
	int	pos = 0,
		col = 0,
		nd_cnt = 0,
		word_break = 0,
		start = 0,
		i,
		len,
		indent = 0,
		beep_cnt = 0,
		beep_max,
		tab_cnt = 0,
		tab_max,
		line = 0;

	*buffer = 0;
	for (i = 0; i < MAXIMUM_SPLITS; i++)
		new_free(&output[i]);
	if (!*str)
		malloc_strcpy(&str, space);	/* special case to make blank lines show up */
	beep_max = get_int_var(BEEP_VAR) ? get_int_var(BEEP_MAX_VAR) : -1;
	tab_max = get_int_var(TAB_VAR) ? get_int_var(TAB_MAX_VAR) : -1;

	for (ptr = (u_char *) str; *ptr && (pos < BIG_BUFFER_SIZE - 10); ptr++)
	{
		if (*ptr <= 32)
		{
			switch (*ptr)
			{
			case '\007':	/* bell */
				if (beep_max == -1)
				{
					buffer[pos++] = REV_TOG;
					buffer[pos++] = (*ptr & 127) | 64;
					buffer[pos++] = REV_TOG;
					nd_cnt += 2;
					col++;
				}
				else if (!beep_max || (++beep_cnt <= beep_max))
				{
					buffer[pos++] = *ptr;
					nd_cnt++;
					col++;
				}
				break;
			case '\011':	/* tab */
				if (tab_max && (++tab_cnt > tab_max))
				{
					buffer[pos++] = REV_TOG;
					buffer[pos++] = (*ptr & 127) | 64;
					buffer[pos++] = REV_TOG;
					nd_cnt += 2;
					col++;
				}
				else
				{
					if (indent == 0)
						indent = -1;
					len = 8 - (col % 8);
					word_break = pos;
					for (i = 0; i < len; i++)
						buffer[pos++] = ' ';
					col += len;
				}
				break;
			case ' ':	/* word break */
				if (indent == 0)
					indent = -1;
				word_break = pos;
				buffer[pos++] = *ptr;
				col++;
				break;
			case UND_TOG:
			case ALL_OFF:
			case REV_TOG:
			case BOLD_TOG:
				buffer[pos++] = *ptr;
				nd_cnt++;
				break;
			default:	/* Anything else, make it displayable */
				if (indent == -1)
					indent = pos - nd_cnt;

				if (get_int_var(DISPLAY_ANSI_VAR) == 0)
				{
					buffer[pos++] = REV_TOG;
					buffer[pos++] = (*ptr & 127) | 64;
					buffer[pos++] = REV_TOG;
					nd_cnt += 2;
					col++;
				}
				/* If we are displaying ansi characters, then
				 * we need to ask the decoder if the character
				 * we are about to use is printable.  If it is
				 * not, then we increment the no_display counter
				 * if it is printable, we increment the column
				 * variable.
				 */
				else
				{
					buffer[pos++] = *ptr;
					if (vt100_decode(*ptr))
						nd_cnt++;
					else
						/* i doubt this is true */
						col++;
				}
				break;
			}
		} 
		else 
		{
			if (indent == -1)
				indent = pos - nd_cnt;
			buffer[pos++] = *ptr;
			if (vt100_decode(*ptr))
				nd_cnt++;
			else
				col++;
		}

		if (pos == BIG_BUFFER_SIZE)
			*ptr = '\0';
		if (col >= CO)
		{
			/* one big long line, no word breaks */
			if (word_break == 0)
				word_break = pos - (col - CO);
			c = buffer[word_break];
			buffer[word_break] = '\0';
			if (cont)
			{
				malloc_strcpy(&temp, cont);
				malloc_strcat(&temp, &(buffer[start]));
			}
			else
				malloc_strcpy(&temp, &(buffer[start]));
			malloc_strcpy(&output[line++], temp);
			buffer[word_break] = c;
			start = word_break;
			word_break = 0;
			while (buffer[start] == ' ')
				start++;
			if (start > pos)
				start = pos;
			if (!(cont_ptr = get_string_var(CONTINUED_LINE_VAR)))
				cont_ptr = empty_string;
			if (get_int_var(INDENT_VAR) && (indent < CO / 3))
			{
		/*
		 * INDENT thanks to Carlo "lynx" v. Loesch
		 * - hehe, nice to see this is still here... -lynx 91
		 */
				if (!cont)
				{
					if ((len = strlen(cont_ptr)) > indent)
					{
						cont = (char *) new_malloc(len + 1);
						strcpy(cont, cont_ptr);
					}
					else
					{
						cont = (char *)	new_malloc(indent + 1);
						strcpy(cont, cont_ptr);
						for (i = len; i < indent; i++)
							cont[i] = ' ';
						cont[indent] = '\0';
					}
				}
			}
			else
				malloc_strcpy(&cont, cont_ptr);
			col = strlen(cont) + (pos - start);
			if (get_int_var(DISPLAY_ANSI_VAR))
				col -= count_ansi(&buffer[start], pos-start);
		}
	}
	buffer[pos] = '\0';
	if (buffer[start])
	{
		if (cont)
		{
			malloc_strcpy(&temp, cont);
			malloc_strcat(&temp, &(buffer[start]));
		}
		else
			malloc_strcpy(&temp, &(buffer[start]));
		malloc_strcpy(&output[line++], temp);
	}
	new_free(&cont);
	new_free(&temp);
	return output;
}

/*
 * add_to_window: adds the given string to the display.  No kidding. This
 * routine handles the whole ball of wax.  It keeps track of what's on the
 * screen, does the scrolling, everything... well, not quite everything...
 * The CONTINUED_LINE idea thanks to Jan L. Peterson (jlp@hamblin.byu.edu)  
 *
 * At least it used to. Now most of this is done by split_up_line, and this
 * function just dumps it onto the screen. This is because the scrollback
 * functions need to be able to figure out how to split things up too.
 */
static	void
add_to_window(window, str)
	Window	*window;
	char	*str;
{

	if (do_hook(WINDOW_LIST, "%u %s", window->refnum, str))
	{
		char	**lines;
		int	logged;

		add_to_log(window->log_fp, str);
		add_to_lastlog(window, str);
		display_highlight(OFF);
		display_bold(OFF);
		strmcat(str, global_all_off, BIG_BUFFER_SIZE - 1);
		logged = islogged(window);
		for (lines = split_up_line(str); *lines; lines++)
		{
			rite(window, *lines, 0, 0, 0, logged);
			if (logged == 1)
				logged = 2;
		}
		term_flush();
	}
}

/*
 * add_to_screen: This adds the given null terminated buffer to the screen.
 * That is, it determines which window the information should go to, which
 * lastlog the information should be added to, which log the information
 * should be sent to, etc 
 */
#ifdef __STDC__
void add_to_screen(char *buffer)
#else
void add_to_screen(buffer)
	char	*buffer;
#endif
{
	Window	*tmp = NULL;
	int	flag;

	if ((buffer[strlen(buffer)-1] == '') || (buffer[strlen(buffer)] == '\r'))
		buffer[strlen(buffer)-1] = '\0'; 

	if (!get_int_var(DISPLAY_ANSI_VAR))
		strcpy(buffer, stripansicodes(buffer));

#ifdef WANT_TCL
	if (in_add_to_tcl)
	{
		add_to_tcl(tmp, buffer);
		return;
	}
#endif
	/* Handles output redirection first */
	if (current_screen->redirect_name && from_server == current_screen->redirect_server)
		send_text(current_screen->redirect_name, buffer, NULL, 0, 0);

	if (dumb)
	{
		add_to_lastlog(curr_scr_win, buffer);
		if (do_hook(WINDOW_LIST, "%u %s", curr_scr_win->refnum, buffer))
			puts(buffer);
		fflush(current_screen->fpout);
		return;
	}
	if (in_window_command)
		update_all_windows();

	if ((who_level == LOG_CURRENT) && (curr_scr_win->server == from_server))
	{
		add_to_window(curr_scr_win, buffer);
		return;
	}
	if (to_window)
	{
		add_to_window(to_window, buffer);
		return;
	}
	if (who_from)
	{
		flag = 1;
		while ((tmp = traverse_all_windows(&flag)) != NULL)
		{
			if (tmp->current_channel && !my_stricmp(who_from, tmp->current_channel))
			{
				if (tmp->server == from_server)
				{
					add_to_window(tmp, buffer);
					return;
				}
			}
			if (tmp->query_nick &&
				(((who_level == LOG_MSG || who_level == LOG_NOTICE)
				&& !my_stricmp(who_from, tmp->query_nick) &&
				from_server == tmp->server) ||
				(who_level == LOG_DCC &&
				(*tmp->query_nick == '=' || *tmp->query_nick == '@') &&
				my_stricmp(who_from, tmp->query_nick + 1) == 0)))
			{
				add_to_window(tmp, buffer);
				return;
			}
		}
		if (is_channel(who_from) && from_server != -1)
		{
			ChannelList *chan;
			if ((chan = (ChannelList *)find_in_list((List **)&(server_list[from_server].chan_list), 
				who_from, !USE_WILDCARDS)))
			{
				if (chan->window && from_server == chan->window->server)
				{
					add_to_window(chan->window, buffer);
					return;
				}
			}
		}
		flag = 1;
		while ((tmp = traverse_all_windows(&flag)) != NULL)
		{
			if (from_server == tmp->server)
			{
				if (find_in_list((List **)&(tmp->nicks), 
					who_from, !USE_WILDCARDS))
				{
					add_to_window(tmp, buffer);
					return;
				}
			}
		}
	}
	flag = 1;
	while ((tmp = traverse_all_windows(&flag)) != NULL)
	{
		if (((from_server == tmp->server) || (from_server == -1)) &&
		    (who_level & tmp->window_level))
		{
			add_to_window(tmp, buffer);
			return;
		}
	}
	if (from_server == curr_scr_win->server)
		tmp = curr_scr_win;
	else
	{
		flag = 1;
		while ((tmp = traverse_all_windows(&flag)) != NULL)
		{
			if (tmp->server == from_server)
				break;
		}
		if (!tmp)
			tmp = curr_scr_win;
	}
	add_to_window(tmp, buffer);
}

/*
 * update_all_windows: This goes through each visible window and draws the
 * necessary portions according the the update field of the window. 
 */
void
update_all_windows()
{
	Window	*tmp;
	int	fast_window,
		full_window,
		r_status,
		u_status;

	for (tmp = current_screen->window_list; tmp; tmp = tmp->next)
	{
		if (tmp->display_size != tmp->old_size)
			resize_display(tmp);
		if (tmp->update)
		{
			fast_window = tmp->update & REDRAW_DISPLAY_FAST;
			full_window = tmp->update & REDRAW_DISPLAY_FULL;
			r_status = tmp->update & REDRAW_STATUS;
			u_status = tmp->update & UPDATE_STATUS;
			if (full_window)
				redraw_window(tmp, 1);
			else if (fast_window)
				redraw_window(tmp, 0);
			if (r_status)
				update_window_status(tmp, 1);
			else if (u_status)
				update_window_status(tmp, 0);
		}
		tmp->update = 0;
	}
	for (tmp = invisible_list; tmp; tmp = tmp->next)
	{
		if (tmp->display_size != tmp->old_size)
			resize_display(tmp);
		tmp->update = 0;
	}
	update_input(UPDATE_JUST_CURSOR);
	term_flush();
}

/*
 * create_refnum: this generates a reference number for a new window that is
 * not currently is use by another window.  A refnum of 0 is reserved (and
 * never returned by this routine).  Using a refnum of 0 in the message_to()
 * routine means no particular window (stuff goes to CRAP) 
 */
static	u_int create_refnum _((void))
{
	unsigned int	new_refnum = 1;
	Window	*tmp;
	int	done = 0,
		flag;

	while (!done)
	{
		done = 1;
		if (new_refnum == 0)
			new_refnum++;

		flag = 1;
		while ((tmp = traverse_all_windows(&flag)) != NULL)
		{
			if (tmp->refnum == new_refnum)
			{
				done = 0;
				new_refnum++;
				break;
			}
		}
	}
	return (new_refnum);
}

/*
 * new_window: This creates a new window on the screen.  It does so by either
 * splitting the current window, or if it can't do that, it splits the
 * largest window.  The new window is added to the window list and made the
 * current window 
 */
Window	*
new_window()
{
	Window	*new;
	static	int	no_screens = 1;

	if (no_screens)

	{
		set_current_screen(create_new_screen());
		main_screen = current_screen;
		no_screens = 0;
	}
	if (dumb && (current_screen->visible_windows == 1))
		return NULL;
	new = (Window *) new_malloc(sizeof(Window));
	bzero(new, sizeof(Window));

	new->refnum = create_refnum();
	if (curr_scr_win)
		new->server = curr_scr_win->server;
	else
		new->server = primary_server;

	new->prev_server = -1;

	if (current_screen->visible_windows)
		new->window_level = LOG_NONE;
	
	new->hold_mode = get_int_var(HOLD_MODE_VAR);
	new->scroll = get_int_var(SCROLL_VAR);
	new->lastlog_level = real_lastlog_level();

#if 0
	if (get_int_var(DOUBLE_STATUS_LINE_VAR) > 2)
		set_int_var(DOUBLE_STATUS_LINE_VAR, 2);
		
#endif
	if (new->refnum <= 1)
		new->double_status = 1;
	else
		new->double_status = 0;

	new->display_size = 1;
	new->old_size = 1;
	new->visible = 1;
	new->screen = current_screen;
	new->notify_level = real_notify_level();
	resize_display(new);
	if (add_to_window_list(new))
		set_current_window(new);
	init_window_variables(new);
	term_flush();
	return (new);
}

void
close_all_screen()
{
	Screen *screen;

	for (screen = screen_list; screen && screen != current_screen;
			screen = screen->next)
	{
		if (screen->alive && screen->fdin != 0)
		{
			if (use_input)
				FD_CLR(screen->fdin, &readables);
			new_close(screen->fdin);
		}
	}
}


static	char	*
next_line_back(window)
	Window	*window;
{
	static	int	row;
	static	Lastlog	*LogLine;
	char	**TheirLines;
	static	char	*ScreenLines[MAXIMUM_SPLITS] =
	{ 
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
#if 0
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL,
		NULL, NULL, NULL, NULL
#endif
	};

	if (window)
	{
		LogLine = window->lastlog_head;
		row = -1;
	}
	if (row <= 0)
	{
		for (row = 0; ScreenLines[row]; row++)
			new_free(&ScreenLines[row]);
		if (!window && LogLine)
			LogLine = LogLine->next;
		if (!LogLine)
			return NULL;
		TheirLines = split_up_line(LogLine->msg);
		for (row = 0; TheirLines[row]; row++)
		{
			ScreenLines[row] = TheirLines[row];
			TheirLines[row] = NULL;
		}
		if (window)
			return NULL;
	}
	return ScreenLines[--row];
}

static	void
display_lastlog_lines(start, end, window)
	int	start,
		end;
	Window	*window;
{
	Display	*Disp;
	char	*Line;
	int	i;

	(void)next_line_back(window);

	for (i = window->new_scrolled_lines; i--;)
		(void)next_line_back(NULL);

	for (i = 0, Disp = window->top_of_display; i < window->display_size;
			Disp = Disp->next, i++)
		if (Disp->linetype)
			(void)next_line_back(NULL);

	for (i = 0; i < start; i++)
		(void)next_line_back(NULL);

	for (; i < end; i++)
	{
		if (!(Line = next_line_back(NULL)))
			break;
		term_move_cursor(0, window->top + window->menu.lines/* + window->status_split*/ +
			window->scrolled_lines - i - 1);
		rite(window, Line, 0, 0, 1, 0);
	}
}

static	void
scrollback_backwards_lines(ScrollDist)
	int	ScrollDist;
{
	Window	*window;

	window = curr_scr_win;
	if (!window->scrolled_lines && !window->scroll)
	{
		term_beep();
		return;
	}
	window->scrolled_lines += ScrollDist;

	term_scroll(window->top + window->menu.lines/* + window->status_split*/, 
		window->top + window->menu.lines + window->display_size - 1 /*+ window->status_split*/, 
		-ScrollDist);

	display_lastlog_lines(window->scrolled_lines - ScrollDist, 
		window->scrolled_lines, window);
	cursor_not_in_display();
	update_input(UPDATE_JUST_CURSOR);
}

static	void
scrollback_forwards_lines(ScrollDist)
	int	ScrollDist;
{
	Window	*window;

	window = curr_scr_win;
	if (!window->scrolled_lines)
	{
		term_beep();
		return;
	}
	if (ScrollDist > window->scrolled_lines)
		ScrollDist = window->scrolled_lines;

	window->scrolled_lines -= ScrollDist;
	term_scroll(window->top + window->menu.lines /*+ window->status_split*/, 
	    window->top + window->menu.lines + window->display_size - 1 /*+ window->status_split*/, 
	    ScrollDist);

	if (window->scrolled_lines < window->display_size)
		redraw_window(window, ScrollDist + window->scrolled_lines - 
				window->display_size);

	display_lastlog_lines(window->scrolled_lines-window->display_size,
	    window->scrolled_lines - window->display_size + ScrollDist, 
		window);
	cursor_not_in_display();
	update_input(UPDATE_JUST_CURSOR);

	if (!window->scrolled_lines)
	{
		window->new_scrolled_lines = 0;
		if (window->hold_mode)
			hold_mode(window, ON, 1);
		else
			hold_mode(window, OFF, 0);
	}
}

void
#ifdef __STDC__
scrollback_forwards(char key, char *ptr)
#else
scrollback_forwards(key, ptr)
	char	key;
	char *	ptr;
#endif
{
	scrollback_forwards_lines(curr_scr_win->display_size/2);
}

void
#ifdef __STDC__
scrollback_backwards(char key, char *ptr)
#else
scrollback_backwards(key, ptr)
	char	key;
	char *	ptr;
#endif
{
	scrollback_backwards_lines(curr_scr_win->display_size/2);
}


void
#ifdef __STDC__
scrollback_end(char key, char *ptr)
#else
scrollback_end(key, ptr)
	char	key;
	char *	ptr;
#endif
{
	Window	*window;

	window = curr_scr_win;
	window->new_scrolled_lines = 0;

	if (!window->scrolled_lines)
	{
		term_beep();
		return;
	}
	if (window->scrolled_lines < window->display_size)
		scrollback_forwards_lines(window->scrolled_lines);
	else
	{
		window->scrolled_lines = 0;
		redraw_window(window, 1);
		cursor_not_in_display();
		update_input(UPDATE_JUST_CURSOR);
		if (window->hold_mode)
			hold_mode(window, ON, 1);
		else
			hold_mode(window, OFF, 0);
	}
}

/*
 * scrollback_start: moves the current screen back to the start of
 * the scrollback buffer..  there are probably cases this doesn't
 * quite work.. -phone, april 1993.
 */
void
#ifdef __STDC__
scrollback_start(char key, char *ptr)
#else
scrollback_start(key, ptr)
	char	key;
	char *	ptr;
#endif
{
	Window	*window;

	window = curr_scr_win;
	if (!window->lastlog_size)
	{
		term_beep();
		return;
	}

	if (window->lastlog_size < window->display_size)
		scrollback_backwards_lines(window->lastlog_size);
	else
	{
		window->scrolled_lines = window->lastlog_size;
		display_lastlog_lines(window->scrolled_lines -
			window->display_size, window->scrolled_lines, window);
		cursor_not_in_display();
		update_input(UPDATE_JUST_CURSOR);
		window->new_scrolled_lines = 0;
		if (window->hold_mode)
			hold_mode(window, ON, 1);
		else
			hold_mode(window, OFF, 0);
	}
}

RETSIGTYPE sig_refresh_screen _((int unused))
{
	refresh_screen(0, NULL);
}

#if 0
#ifdef WINDOW_CREATE
extern	Window	*create_additional_screen _((void))
{
        Window  *win;
        Screen  *oldscreen;
        char    *displayvar,
                *termvar;
        int     screen_type = ST_NOTHING;
        struct  sockaddr_un sock,
                        *sockaddr = &sock,
                        NewSock;
        int     NsZ;
        int     s;
	fd_set	fd_read;
	struct	timeval	timeout;
	pid_t	child;
	int	old_timeout;
	char buffer[BIG_BUFFER_SIZE + 1];


	if (!use_input)
		return NULL;


	/* Check for X first. */
	if ((displayvar = getenv("DISPLAY")))
	{
		if (!(termvar = getenv("TERM")))
			say("I don't know how to create new windows for this terminal");
		else
			screen_type = ST_XTERM;
	}
	/*
	 * Environment variable STY has to be set for screen to work..  so it is
	 * the best way to check screen..  regardless of what TERM is, the 
	 * execpl() for screen won't just open a new window if STY isn't set,
	 * it will open a new screen process, and run the wserv in its first
	 * window, not what we want...  -phone
	 */
	if (screen_type == ST_NOTHING && getenv("STY"))
		screen_type = ST_SCREEN;


	if (screen_type == ST_NOTHING)
	{
		say("I don't know how to create new windows for this terminal");
		return (Window *) 0;
	}

        say("Opening new %s...",
                screen_type == ST_XTERM ?  "window" :
                screen_type == ST_SCREEN ? "screen" :
                                           "wound" );

        sprintf(sock.sun_path, "/tmp/irc_%08ld", (long int)getpid());
        sock.sun_family = AF_UNIX;
        s = socket(AF_UNIX, SOCK_STREAM, 0);
        bind(s, (struct sockaddr *) &sock, 2 + strlen(sock.sun_path));
	listen(s, 1);
	oldscreen = current_screen;
	set_current_screen(create_new_screen());

	switch ((child = fork()))
	{
		case -1:
		{
			kill_screen(current_screen);
			last_input_screen = oldscreen;
			set_current_screen(oldscreen);
			say("I couldnt fire up a new wserv process");
			break;
		}

		case 0:
		{
			char stuff[256], *opts, *of;
			char *xterm;
			char *args[64], **args_ptr = args;
			char geom[32];

			setuid(getuid());
			setgid(getgid());
		/*
		 * Unlike most other cases, it is important here to close down
		 * *ALL* unneeded file descriptors. Failure to do so can cause
		 * Things like server and DCC connections to fail to close on
		 * request. This isn't a problem with "screen", but is with X.
		 */
			close(s);
			close_all_screen();
			close_all_dcc();
			close_all_exec();
			close_all_server();

			if (screen_type == ST_SCREEN)
			{
				of = opts = m_strdup(get_string_var(SCREEN_OPTIONS_VAR));

				*args_ptr++ = "screen";
				while (opts && *opts)
					*args_ptr++ = next_arg(opts, &opts);
			}
			else if (screen_type == ST_XTERM)
			{
				sprintf(geom, "%dx%d", CO+1, LI);
				of = opts = m_strdup(get_string_var(XTERM_OPTIONS_VAR));
				if (!(xterm = getenv("XTERM")))
					xterm = "xterm";

				*args_ptr++ = xterm;
				*args_ptr++ = "-geometry";
				*args_ptr++ = geom;
				while (opts && *opts)
					*args_ptr++ = next_arg(opts, &opts);
				*args_ptr++ = "-e";
			}

			*args_ptr++ = WSERV_PATH;
			*args_ptr++ = sockaddr->sun_path;
			*args_ptr++ = NULL;
			new_free(&of);

			s = execvp(args[0], args);
			printf("aaaaaaaaaaa! [%d/%s]\n", s, strerror(errno));
			exit(0);
		}
	}

	/* All the rest of this is the parent.... */
	NsZ = sizeof(NewSock);
	FD_ZERO(&fd_read);
	FD_SET(s, &fd_read);
	timeout.tv_sec = (time_t) 10;
	timeout.tv_usec = 0;
	sleep (1);

	/* using say(), yell() can be bad in this next section of code. */
	switch (select(NFDBITS , &fd_read, NULL, NULL, &timeout))
	{
	case -1:
	case 0:
	{
		int errnod = get_child_exit (child);
		close(s);
		kill_screen(current_screen);
		kill(child, SIGKILL);
		last_input_screen = oldscreen;
		set_current_screen(oldscreen);
                yell("child %s with %d", (errnod < 1) ? "signaled" : "exited",
                                         (errnod < 1) ? -errnod : errnod);
		yell("Errno is %d", errno);
		return NULL;
	}
	default:
	{
		current_screen->fdin = current_screen->fdout = accept(s, (struct sockaddr *) &NewSock, &NsZ);
		if (current_screen->fdin < 0)
			return (Window *) 0;
		FD_SET(current_screen->fdin, &readables);
		current_screen->fpin = current_screen->fpout = fdopen(current_screen->fdin, "r+");
		close(s);
		unlink(sockaddr->sun_path);
		old_timeout = dgets_timeout(5);
		if (dgets(buffer, BIG_BUFFER_SIZE, current_screen->fdin, NULL) < 1)
		{
			FD_CLR(current_screen->fdin, &readables);
			close(current_screen->fdin);
			kill_screen(current_screen);
			kill(child, SIGKILL);
			last_input_screen = oldscreen;
			set_current_screen(oldscreen);
			(void) dgets_timeout(old_timeout);
			return NULL;
		}
		else
			malloc_strcpy(&current_screen->tty_name, buffer);

		win = new_window();
		(void) refresh_screen(0, NULL);
		set_current_screen(oldscreen);
		(void) dgets_timeout(old_timeout);
		return win;
	}
	}
	return NULL;
}
#endif /* WINDOW_CREATE */
#endif


void set_screens _((fd_set *rd, fd_set *wd))
{
Screen *screen;
	if (use_input)
		for (screen = screen_list; screen; screen = screen->next)
			if (screen->alive)
				FD_SET(screen->fdin, rd);
}



/* cough cough */
extern int key_pressed;

void do_screens _((fd_set *rd))
{
	Screen *screen;
	char buffer[BIG_BUFFER_SIZE + 1];

	if (use_input)
	for (screen = screen_list; screen; screen = screen->next)
	{
		if (!screen->alive)
			continue;
		set_current_screen(screen);
		if (FD_ISSET(screen->fdin, rd))
		{
			/*
			 * This section of code handles all in put from the terminal(s).
			 * connected to ircII.  Perhaps the idle time *shouldn't* be 
			 * reset unless its not a screen-fd that was closed..
			 */
			idle_time = time(NULL);
			if (dumb)
			{
				int     old_timeout;

				old_timeout = dgets_timeout(1);
				if (dgets(buffer, INPUT_BUFFER_SIZE, screen->fdin, NULL))
				{
					(void) dgets_timeout(old_timeout);
					*(buffer + strlen(buffer) - 1) = '\0';
					if (get_int_var(INPUT_ALIASES_VAR))
						parse_line(NULL, buffer, empty_string, 1, 0);
					else
						parse_line(NULL, buffer, NULL, 1, 0);
				}
				else
				{
					say("IRCII exiting on EOF from stdin");
					irc_exit(1, "BitchX - EOF from stdin");
				}
				key_pressed = 13; /* *cough* *cough* */
			}
			else
			{
				int server;
				char	loc_buffer[BIG_BUFFER_SIZE + 1];
				int	n, i;

				server = from_server;
				from_server = get_window_server(0);
				last_input_screen = screen;
				if ((n = read(screen->fdin, loc_buffer, BIG_BUFFER_SIZE)) != 0)
				{
					extended_handled = 0;
					for (i = 0; i < n; i++)
						if (!extended_handled)
							edit_char(loc_buffer[i]);
					key_pressed = loc_buffer[0];
				}
				from_server = server;
			} 
		}
	} 
} 
