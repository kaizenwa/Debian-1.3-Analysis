/*
 * help.c: handles the help stuff for irc 
 *
 * Written by Michael Sandrof
 * Extensively modified by Troy Rollo
 * Re-modified by Matthew Green
 *
 * Copyright(c) 1992 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

/*
 * This has been replaced almost entirely from the original by Michael
 * Sandrof in order to fit in with the multiple screen code.
 *
 * ugh, this wasn't easy to do, but I got there, after working out what
 * had been changed and why, by myself - phone, October 1992.
 *
 * And when I started getting /window create working, I discovered new
 * bugs, and there has been a few more major changes in here again.
 * It is illegal to call help from more than one screen, at the moment,
 * because there is to much to keep track of - phone, jan 1993.
 */

#include "irc.h"

/* stuff from gnu autoconf docs */

#ifdef NeXT		/* ugly hack 'cause configure don't grok it -phone */
# define SYSDIR
#endif

#if defined(HAVE_DIRENT_H) || defined(_POSIX_SOURCE)
# include <dirent.h>
#else
# define dirent direct
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#include <sys/stat.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "help.h"
#include "input.h"
#include "ircaux.h"
#include "output.h"
#include "screen.h"
#include "server.h"
#include "ircterm.h"
#include "vars.h"
#include "window.h"

#if defined(ISC22)
extern char *strrchr();
# define rindex strrchr
# include <sys/dirent.h>
# define direct dirent
#endif /* ISC22 */

/* Forward declarations */

static	void	help_me _((char *, char *));
static	void	help_show_paused_topic _((char *, char *));
static	void	create_help_window _((void));
static	void	set_help_screen _((Screen *));

/*
 * A few variables here - A lot added to get help working with
 * non - recursive calls to irc_io, and also have it still 
 * reading things from the server(s), so not to ping timeout.
 */
static	int	dont_pause_topic = 0;
static	int	entry_size;
static	int	finished_help_paging = 0;
static	FILE *	help_fp = NULL;
static	int	help_paused_lines;
static	char *	help_paused_topic[256];		/* 100 should be enough */
static	Screen *help_screen = NULL;
static	int	help_show_directory = 0;
static	char	help_topic_list[BIG_BUFFER_SIZE + 1];
static	Window *help_window = NULL;
static	char	no_help[] = "NOHELP";
static	char	paused_topic[128];
static	char *	this_arg;
static	int	use_help_window = 0;


/* compar: used by scandir to alphabetize the help entries */
static int	compar (struct dirent **e1, struct dirent **e2)
{
	return (my_stricmp((*e1)->d_name, (*e2)->d_name));
}

/*
 * selectent: used by scandir to decide which entries to include in the help
 * listing.  
 */
static	int	selectent (struct dirent *entry)
{
	if (*(entry->d_name) == '.')
		return (0);
	if (my_strnicmp(entry->d_name, this_arg, strlen(this_arg)))
		return (0);
	else
	{
		int len = strlen(entry->d_name);
		char *temp = entry->d_name;

	/*
	 * See how long the uncompressed part of the filename is..
 	 * Before the scandir call, entry_size is set to 0, and this
	 * right here will (eventually) set entry_size to the filename
	 * with the longest length (not counting compression suffixes)
	 * which will then be used as the column width.  Slick.
	 */
		if (! end_strcmp (temp, ".gz", 3))
			len -=3;
		entry_size = (len > entry_size) ? len : entry_size;
		return (1);
	}
}

/* 
 * show_help:  show's either a page of text from a help_fp, or the whole
 * thing, depending on the value of HELP_PAGER_VAR.  If it gets to the end,
 * (in either case it will eventally), it closes the file, and returns 0
 * to indicate this.
 */ 
static	int	show_help (Window *window, char *name)
{
	Window	*old_window;
	int	rows = 0;
	char	line[256];

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
 		if (fgets(line, 255, help_fp))
		{
			if (*(line + strlen(line) - 1) == '\n')
			*(line + strlen(line) - 1) = (char) 0;

			if (*line != '!' || *line !='#')
				help_put_it(name, "%s", line);
			else
				rows++;
		}
		else
		{
			curr_scr_win = old_window;
			if (help_fp) fclose(help_fp);
			help_fp = NULL;
			return (0);
		}
	}
	curr_scr_win = old_window;
	return (1);
	
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
static	void	help_prompt (char *name, char *line)
{
	if (finished_help_paging)
	{
		if (*paused_topic)
			help_show_paused_topic(paused_topic, empty_string);
		return;
	}

	if (line && ((*line == 'q') || (*line == 'Q')))
	{
		finished_help_paging = 1;
		if (help_fp) fclose(help_fp);
		help_fp = NULL;
		set_help_screen(NULL);
		return;
	}

	if (show_help(help_window, name))
	{
		if (dumb)
			help_prompt(name, NULL);
		else
			add_wait_prompt("*** Hit any key for more, 'q' to quit ***",
				help_prompt, name, WAIT_PROMPT_KEY);
	}
	else
	{
		finished_help_paging = 1;
		if (help_fp) fclose(help_fp);
		help_fp = NULL;
		if (help_show_directory)
		{
			if (get_int_var(HELP_PAGER_VAR))
			{
				if (dumb)
					help_show_paused_topic(name, empty_string);
				else
					add_wait_prompt("*** Hit any key to end ***", 
						help_show_paused_topic, paused_topic,
						WAIT_PROMPT_KEY);
			}
			else
			{
				help_show_paused_topic(paused_topic, empty_string);
				set_help_screen(NULL);
			}
			help_show_directory = 0;
			return;
		}
	}

	if (finished_help_paging)
	{
		if (get_int_var(HELP_PROMPT_VAR))
		{
			char	tmp[BIG_BUFFER_SIZE + 1];

			sprintf(tmp, "%s%sHelp? ", help_topic_list,
				*help_topic_list ? " " : empty_string);
			if (!dumb)
				add_wait_prompt(tmp, help_me, help_topic_list,
					WAIT_PROMPT_LINE);
		}
		else
		{
			if (*paused_topic)
				help_show_paused_topic(paused_topic, empty_string);
			set_help_screen(NULL);
		}
	}
}

/*
 * help_topic:  Given a topic, we search the help directory, and try to
 * find the right file, if all is cool, and we can open it, or zcat it,
 * then we call help_prompt to get the actually displaying of the file
 * on the road.
 */
static	void	help_topic (char *path, char *name)
{
	char	*filename = NULL;

	if (name == (char *) 0)
		return;

	/* what is the base name? */
	malloc_sprintf(&filename, "%s/%s", path, name);

	/* let uzfopen have all the fun */
	help_fp = uzfopen (&filename, path);
	if (!help_fp)
	{
		help_put_it (name, "%s No help available on %s: Use ? for list of topics", thing_ansi, name);
		return;
	}
	/* Isnt this a heck of a lot better then the kludge you were using? */
	help_put_it(name, "%s Help on %s", thing_ansi, name);
	help_prompt(name, NULL);
	return;
}

/*
 * help_pause_add_line: this procedure does a help_put_it() call, but
 * puts off the calling, until help_show_paused_topic() is called.
 * I do this because I need to create the list of help topics, but
 * not show them, until we've seen the whole file, so we called
 * help_show_paused_topic() when we've seen the file, if it is needed.
 */
static 	void 	help_pause_add_line (char *format, ...)
{
	char	buf[BIG_BUFFER_SIZE];

	va_list args;
	va_start (args, format);
	vsprintf(buf, format, args);
	va_end (args);
	malloc_strcpy(&help_paused_topic[help_paused_lines], buf);
	help_paused_lines++;
}

/*
 * help_show_paused_topic:  see above.  Called when we've seen the
 * whole help file, and we have a list of topics to display.
 */
static	void	help_show_paused_topic (char *name, char *line)
{
	static int i = 0;
	int j = 0;
	int rows;

	if (!help_paused_lines)
		return;

	if ((*line == 'q') || (*line == 'Q'))
		i = help_paused_lines + 1;	/* just big enough */

	rows = help_window->display_size;
	if (i < help_paused_lines)
	{
		for (j = 0; j < rows; j++)
		{
			help_put_it (name, "%s", help_paused_topic[i]);
			new_free(&help_paused_topic[i]);

			/* if we're done, the recurse to break loop */
			if (++i >= help_paused_lines)
				break;
		}
		if (!dumb)
		{
			if ((i < help_paused_lines) && get_int_var(HELP_PAGER_VAR))
				add_wait_prompt("[MORE]", help_show_paused_topic, name, WAIT_PROMPT_KEY);
		}
		else
			help_show_paused_topic(name, line);
	}

	/* this cant be an else of the previous if because 'i' can 
	 * change in the previous if and we need to test it again
	 */
	if (i >= help_paused_lines)
	{
		if (get_int_var(HELP_PROMPT_VAR))
		{
			char	buf[BIG_BUFFER_SIZE];

			sprintf(buf, "%s%sHelp? ", name, (name && *name) ? " " : empty_string);
			if (!dumb)
				add_wait_prompt(buf, help_me, name, WAIT_PROMPT_LINE);
		}
		else
			set_help_screen(NULL);

		dont_pause_topic = 0;
		help_paused_lines = 0;
		i = 0;
	}
}

/*
 * help_me:  The big one.  The help procedure that handles working out
 * what was actually requested, sets up the paused topic list if it is
 * needed, does pretty much all the hard work.
 */
static	void	help_me (char *topics, char *args)
{
	char *	ptr;
	struct	dirent	**namelist = NULL;
	int	entries,
		free_cnt = 0,
		cnt,
		i,
		cols;
	struct	stat	stat_buf;
	char	path[BIG_BUFFER_SIZE+1];
	int	help_paused_first_call = 0;
	char *	help_paused_path = NULL;
	char *	help_paused_name = NULL;
	char *	temp;
	char	tmp[BIG_BUFFER_SIZE+1];
	char	buffer[BIG_BUFFER_SIZE+1];

	strcpy(help_topic_list, topics);
	ptr = get_string_var(HELP_PATH_VAR);

	sprintf(path, "%s/%s", ptr, topics);
	for (ptr = path; (ptr = index(ptr, ' '));)
		*ptr = '/';

	/*
	 * first we check access to the help dir, whinge if we can't, then
	 * work out we need to ask them for more help, else we check the
	 * args list, and do the stuff 
	 */
	if (help_show_directory)
	{
		help_show_paused_topic(paused_topic, empty_string);
		help_show_directory = 0;
	}
		
	finished_help_paging = 0;
	if (access(path, R_OK|X_OK))
	{
		help_put_it(no_help, "%s Cannot access help directory!", thing_ansi);
		set_help_screen(NULL);
		return;
	}

	this_arg = next_arg(args, &args);
	if (!this_arg && *help_topic_list && get_int_var(HELP_PROMPT_VAR))
	{
		if ((temp = rindex(help_topic_list, ' ')) != NULL)
			*temp = '\0';
		else
			*help_topic_list = '\0';

		sprintf(tmp, "%s%sHelp? ", help_topic_list, *help_topic_list ? " " : empty_string);

		if (!dumb)
			add_wait_prompt(tmp, help_me, help_topic_list, WAIT_PROMPT_LINE);
		return;
	}

	if (!this_arg)
	{
		set_help_screen(NULL);
		return;
	}

	create_help_window();

	/*
	 * This is just a bogus while loop which is intended to allow
	 * the user to do '/help alias expressions' without having to
	 * include a slash inbetween the topic and subtopic.
	 *
	 * If all goes well, we 'break' at the bottom of the loop.
	 */
	while (this_arg)
	{
		message_from(NULL, LOG_CURRENT);

		if (!*this_arg)
			help_topic(path, NULL);

		if (strcmp(this_arg, "?") == 0)
		{
			this_arg = empty_string;
			if (!dont_pause_topic)
				dont_pause_topic = 1;
		}

		/*
		 * entry_size is set by our scandir() handler to the width
		 * of the longest help topic (adjusted for compression
		 * extensions, of course.)
		 */
		entry_size = 0;

		/*
		 * Gather up the names of the files in the help directory.
		 * XXXX - this should be done by glob().  Scandir() isnt
		 *        standarized.
		 */
		free_cnt = entries = scandir(path, &namelist, 
			(int (*) _((const struct dirent *))) selectent,
			(const void *)(int (*) _((const struct dirent *, const struct dirent *)))compar);

		/*
		 * Because there are help files which are a full subset
		 * of a the name of another help file (ie, 'LOG' and 
		 * 'LOGFILE'), we need to make sure that if the user said
		 * /help LOG, they really get the help file for LOG and are
		 * not asked to pick between LOG and LOGFILE.
		 *
		 * So:  If there is more then one entry, but one of the
		 * filenames is exactly what the user requested (adjusted
		 * for compression extension, of course), then we use it
		 * and forget the others.
		 */
		if (entries > 1)
		{
			/*
			 * Because the filenames are sorted, we know that
			 * the help topic that the user requested would
			 * definitely be in namelist[0].
			 *
			 * First thing we do is see how many characters
			 * are different between namelist[0] and the topic
			 * we're looking for.
			 */
			int len1 = strlen (namelist[0]->d_name);
			int len2 = strlen (this_arg);
			int len3 = len1 - len2;	/* should be positive */

			/*
			 * If namelist[0] and the help topic match to
			 * the length of the help topic, then if the
			 * difference is exactly the length of a compression
			 * suffix AND that suffix is present, then we have
			 * the topic we're looking for.  We clean off
			 * all the other topics and reset the size.
			 */
			if ((!my_strnicmp(namelist[0]->d_name, this_arg, len2)
				&& !end_strcmp(namelist[0]->d_name, ".gz", len3))
			   || !my_stricmp(namelist[0]->d_name, this_arg))
			{
				entries = 1;
			}
		}

		if (!*help_topic_list)
			dont_pause_topic = 1;

/* reformatted */
/*
 * entries: -1 means something really died, 0 means there
 * was no help, 1, means it wasn't a directory, and so to
 * show the help file, and the default means to add the
 * stuff to the paused topic list..
 */
switch (entries)
{
	case -1:
	{
		help_put_it(no_help, "%s Error during help function: %s", thing_ansi, sys_errlist[errno]);
		set_help_screen(NULL);
		if (help_paused_first_call)
		{
			help_topic(help_paused_path, help_paused_name);
			help_paused_first_call = 0;
			new_free(&help_paused_path);
			new_free(&help_paused_name);
		}
		return;
	}
	case 0:
	{
		help_put_it(this_arg, "%s No help available on %s: Use ? for list of topics", thing_ansi, this_arg);
		if (!get_int_var(HELP_PROMPT_VAR))
		{
			set_help_screen(NULL);
			break;
		}
		sprintf(tmp, "%s%sHelp? ", help_topic_list, *help_topic_list ? " " : empty_string);
		if (!dumb)
			add_wait_prompt(tmp, help_me, help_topic_list, WAIT_PROMPT_LINE);

		if (help_paused_first_call)
		{
			help_topic(help_paused_path, help_paused_name);
			help_paused_first_call = 0;
			new_free(&help_paused_path);
			new_free(&help_paused_name);
		}
		break;
	}
	case 1:
	{
		sprintf(tmp, "%s/%s", path, namelist[0]->d_name);
		stat_file(tmp, &stat_buf);
		if (stat_buf.st_mode & S_IFDIR)
		{
			strcpy(path, tmp);
			if (*help_topic_list)
				strcat(help_topic_list, " ");

			strcat(help_topic_list, namelist[0]->d_name);
			if ((this_arg = next_arg(args, &args)) == (char *) 0)
			{
				help_paused_first_call = 1;
				malloc_strcpy(&help_paused_path, path);
				malloc_strcpy(&help_paused_name, namelist[0]->d_name);
				dont_pause_topic = -1;
				this_arg = "?";
			}
			/*
			 * Why do we free this here, like this?
			 * Because we're about to continue, which means
			 * we hop back up to the top of the loop:  We will
			 * miss the cleanup that happens just after the end
 			 * of this case statement, and so we must do the
 			 * cleanup to avoid the memory leak.  We use free_cnt,
			 * because it is the 'true' measure of what is in
			 * the array (entries might have been modified above).
			 */
			for (i = 0; i < /*free_cnt*/ entries; i++)
				free((char *)namelist[i]);
			free((char *)namelist);
			namelist = NULL;
			continue;
		}
		else
		{
			help_topic(path, namelist[0]->d_name);
			finished_help_paging = 0;	/* this is a big kludge */
			break;
		}
	}
	default:
	{
		help_show_directory = 1;
		strcpy(paused_topic, help_topic_list);
		help_pause_add_line("%s %s choices:", thing_ansi, help_topic_list);
		entry_size += 2;
		cols = (CO - 10) / entry_size;

		strcpy(buffer, empty_string);
		cnt = 0;

		for (i = 0; i < entries; i++)
		{
			/*
			 * Here we remove the compression suffix (if any)
			 * from the filename, since we're not interested in
			 * it at this point.
			 */
			if (!end_strcmp(namelist[i]->d_name, ".gz", 3))
				chop(namelist[i]->d_name, 3);

			/*
			 * Then we append the help topic to the current
			 * output line.
			 */
			strcat(buffer, namelist[i]->d_name);

			/*
			 * Since we already know how many columns each
			 * line will contain, we check to see if we have
			 * accumulated that many entries.  If we have, we
			 * output the line to the screen.
			 */
			if (++cnt == cols)
			{
				help_pause_add_line("%s", buffer);
				strcpy(buffer, empty_string);
				cnt = 0;
			}

			/*
			 * If we have not finished this line, then we have
			 * to pad the name length out to the expected width.
			 * 'entry_size' is the column width.  We also have
			 * do adjust for compression extension.
			 */
			else
				strextend(buffer, ' ', entry_size - strlen(namelist[i]->d_name));
		}

		help_pause_add_line("%s", buffer);
		if (help_paused_first_call)
		{
			help_topic(help_paused_path, help_paused_name);
			help_paused_first_call = 0;
			new_free(&help_paused_path);
			new_free(&help_paused_name);
		}
		if (dont_pause_topic == 1)
		{
			help_show_paused_topic(paused_topic, empty_string);
			help_show_directory = 0;
		}
		break;
	}
}
/* end of reformatting */


		/*
		 * We only get here if everything happened ok.  free_cnt
		 * can be -1, 0, or a positive value here, and we know that
		 * if we get here, we havent free()d it yet.  So we free it
		 * here and exit the while loop.
		 */
		if (namelist)
		{
			for (i = 0; i < free_cnt; i++)
				free((char *)namelist[i]);
			free((char *)namelist);
		}
		break;
	}
	/*
	 * This one is for when there was never a topic and the prompt
	 * never got a topic..  and help_screen was never reset..
	 * phone, jan 1993.
	 */
	if (!*help_topic_list && finished_help_paging)
		set_help_screen(NULL);
}

/*
 * help: the HELP command, gives help listings for any and all topics out
 * there 
 */
extern void help (char *command, char *args, char *subargs)
{
	char	*help_path;

	finished_help_paging = 0;
	help_show_directory = 0;
	dont_pause_topic = 0;
	use_help_window = 0;

	/*
	 * The idea here is to work out what sort of help we are using - 
	 * either the installed help files, or some help service, what
	 * ever it maybe.  Once we have worked this out, if we are using
	 * a help window, set it up properly.
	 */

	help_path = get_string_var(HELP_PATH_VAR);

	if (!(help_path && *help_path && !access(help_path, R_OK | X_OK)))
	{
		help_put_it(no_help, "%s HELP_PATH variable not set or set to an invalid path", thing_ansi);
		return;
	}

	/*
	 * Here, if we are using the help files locally, we must ensure that
	 * we aren't doing HELP in a more than one screen - phone, jan 1993.
	 */

	if (help_path && help_screen && help_screen != current_screen)
	{
		say("You may not run help in two screens");
		return;
	}
	help_screen = current_screen;
	help_window = NULL;
	help_me(empty_string, (args && *args) ? args : "?");

}




static	void create_help_window _((void))
{
	if (help_window)
		return;

	if (!dumb && get_int_var(HELP_WINDOW_VAR))
	{
		use_help_window = 1;
		help_window = new_window();

		help_window->hold_mode = OFF;
		update_all_windows();
	}
	else
		help_window = curr_scr_win;
}




static	void	set_help_screen (Screen *screen)
{
	help_screen = screen;
	if (!help_screen && help_window)
	{
		if (use_help_window)
		{
			int display = window_display;

			window_display = 0;
			delete_window(help_window);
			window_display = display;
		}
		help_window = NULL;
		update_all_windows();
	}
}
