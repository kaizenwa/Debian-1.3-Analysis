/* 
 * Copyright Colten Edwards 1996
 */
 
#include "irc.h"

#if defined(HAVE_DIRENT_H) || defined(_POSIX_SOURCE)
# include <dirent.h>
# define NLENGTH(d) (strlen((d)->d_name)
#else /* DIRENT || _POSIX_SOURCE */
# define dirent direct
# define NLENGTH(d) ((d)->d_namlen)
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif /* HAVE_SYS_NDIR_H */
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif /* HAVE_SYS_DIR_H */
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif /* HAVE_NDIR_H */
#endif /* HAVE_DIRENT_H || _POSIX_VERSION */

#include <sys/stat.h>

#include "ircterm.h"
#include "server.h"
#include "vars.h"
#include "ircaux.h"
#include "input.h"
#include "window.h"
#include "screen.h"
#include "output.h"
#include "misc.h"

FILE * msg_fp = NULL;

static  Window  *msg_window = NULL;
static  int     finished_msg_paging = 0;
static  Screen  *msg_screen = NULL;
static  int     use_msg_window = 0;
static	void log_prompt _((char *name, char *line));
static	void set_msg_screen _((Screen *));

void remove_log(char *command, char *args, char *subargs) 
{
	char *expand;
	char *filename = NULL;
	int old_display = window_display;

	int  reset_logptr = 0;

	if ((get_string_var(MSGLOGFILE_VAR) == NULL) || (get_string_var(CTOOLZ_DIR_VAR) == NULL))
		return;
	malloc_sprintf(&filename, "%s/%s", get_string_var(CTOOLZ_DIR_VAR), get_string_var(MSGLOGFILE_VAR));
	expand = expand_twiddle(filename);
	new_free(&filename);
	window_display = 0;	
	reset_logptr = logmsg(0, LOG_CURRENT, NULL, NULL, NULL, NULL, 3);
	log_toggle(0);
	window_display = old_display;
	if (unlink(expand)) {
		bitchsay("Error unlinking: %s", expand);
		new_free(&expand);
		return;
	}
	window_display = 0;
	set_int_var(MSGCOUNT_VAR, 0);
	if (reset_logptr)
		log_toggle(1);
	window_display = old_display;
	bitchsay("Removed %s.", expand);
	new_free(&expand);
}

static int in_read_log = 0;

void readlog(char *command,char *args, char *subargs)
{
	char *expand;
	struct	stat	stat_buf;
	char	*filename = NULL;
	static char buffer[BIG_BUFFER_SIZE + 1];
	
	if (!get_string_var(MSGLOGFILE_VAR))
		if (!args || (args && !*args))
			return;
	if (msg_window)
		return;

	if (command)
		in_read_log = 1;
		
	if (args && *args)
		malloc_sprintf(&filename, "%s", args);
	else
		malloc_sprintf(&filename, "%s/%s", get_string_var(CTOOLZ_DIR_VAR), get_string_var(MSGLOGFILE_VAR));

	expand = expand_twiddle(filename);
	new_free(&filename);
	stat_file(expand, &stat_buf);
	strcpy(buffer, expand);
	
	if (stat_buf.st_mode & S_IFDIR)
		return;

		if ((msg_fp = fopen(expand, "r")) == NULL)
		{
			help_put_it(expand, "%s Error Opening Log file %s", thing_ansi, expand);
			new_free(&expand);
			msg_fp = NULL;
			return;
		}
		new_free(&expand);
	/*
	 * Hopefully now we have got a file descriptor <help_fp>, a name
	 * so we start displaying the help file, calling msg_prompt for
	 * the first time.
	 */
	msg_window = curr_scr_win;
	msg_screen = current_screen;
	log_prompt(buffer, NULL);
}

/*
 * show_help:  show's either a page of text from a help_fp, or the whole
 * thing, depending on the value of HELP_PAGER_VAR.  If it gets to the end,
 * (in either case it will eventally), it closes the file, and returns 0
 * to indicate this.
 */ 
static	int
show_log(Window *window, char *name)
{
	Window	*old_window;
	int	rows = 0;
	char	line[81];

	if (window)
	{
		old_window = curr_scr_win;
		curr_scr_win = window;
	}
	else
	{
		old_window = NULL;
		window = curr_scr_win;
	}
	if (get_int_var(HELP_PAGER_VAR))
		rows = window->display_size;
	while (--rows)
	{
 		if (rfgets(line, 80, msg_fp))
		{
			if (*(line + strlen(line) - 1) == '\n')
			*(line + strlen(line) - 1) = (char) 0;

	/*
	 * I want to remove the else portion of this code, as I
	 * find it offsensive, but too many help files rely on
	 * it.. sigh.. -phone
	 */
			help_put_it(name, "%s", line);
		}
		else
		{
			if (msg_fp) fclose(msg_fp);
			set_msg_screen(NULL);
			msg_fp = NULL;
			return (0);
		}
	}
	return (1);
}

void remove_away_log(char *stuff, char *line)
{
	if ((line && toupper(*line) == 'Y'))
		remove_log(NULL, NULL, NULL);
	in_read_log = 0;
}
                         
static	void
set_msg_screen(screen)
	Screen	*screen;
{
	msg_screen = screen;
	if (!msg_screen && msg_window)
	{
		if (use_msg_window)
		{
			int display = window_display;

			window_display = 0;
			delete_window(msg_window);
			window_display = display;
		}
		msg_window = NULL;
		update_all_windows();
	}
}

/*
 * help_prompt: The main procedure called to display the help file
 * currently being accessed.  Using add_wait_prompt(), it sets it
 * self up to be recalled when the next page is asked for.   If
 * called when we have finished paging the help file, we exit, as
 * there is nothing left to show.  If line is 'q' or 'Q', exit the
 * help pager, clean up, etc..  If all is cool for now, we call
 * show_help, and either if its finished, exit, or prompt for the
 * next page.   From here, if we've finished the help page, and
 * doing help prompts, prompt for the help..
 */

static	void
log_prompt(char *name, char *line)
{

	if (line && ((*line == 'q') || (*line == 'Q')))
	{
		finished_msg_paging = 1;
		if (msg_fp) fclose(msg_fp);
		msg_fp = NULL;
		set_msg_screen(NULL);
		if (!in_read_log)
			add_wait_prompt("Delete msg log [y/N]? ", remove_away_log, "", WAIT_PROMPT_LINE);
		return;
	}

	if (show_log(msg_window, name))
	{
		add_wait_prompt("*** Hit any key for more, 'q' to quit ***",
			log_prompt, name, WAIT_PROMPT_KEY);
	}
	else 
	{
		if (msg_fp)fclose(msg_fp);
		set_msg_screen(NULL);
		msg_fp = NULL;
		if (!in_read_log)
			add_wait_prompt("Delete msg log [y/N]? ", remove_away_log, "", WAIT_PROMPT_LINE);
	}
}

