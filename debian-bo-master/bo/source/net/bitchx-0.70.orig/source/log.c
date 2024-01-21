/*
 * log.c: handles the irc session logging functions 
 *
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include <sys/stat.h>

#include "log.h"
#include "vars.h"
#include "screen.h"
#include "output.h"
#include "ircaux.h"

FILE	*irclog_fp;
extern char *stripansicodes _((char *));

FILE	* do_log(int flag, char *logfile, FILE *fp)
{
	time_t	t;

	if (logfile == NULL)
		return NULL;
	t = time(0);
	if (flag)
	{
		if (fp)
			say("Logging is already on");
		else
		{
#ifdef DAEMON_UID
			if (getuid() == DAEMON_UID)
			{
				say("You are not permitted to use LOG");
				/* fp = (FILE *) 0;  unused */
			}
			else
			{
#endif /* DAEMON_UID */
				say("Starting logfile %s", logfile);
				if ((fp = fopen(logfile, "a")) != NULL)
				{
					chmod(logfile, S_IREAD | S_IWRITE);
					fprintf(fp, "IRC log started %.16s\n", ctime(&t));
					fflush(fp);
				}
				else
				{
					say("Couldn't open logfile %s: %s", logfile, strerror(errno));
					fp = NULL;
				}
#ifdef DAEMON_UID
			}
#endif /* DAEMON_UID */
		}
	}
	else
	{
		if (fp)
		{
			fprintf(fp, "IRC log ended %.16s\n", ctime(&t));
			fflush(fp);
			fclose(fp);
			fp = NULL;
			say("Logfile ended");
		}
	}
	return (fp);
}

/* logger: if flag is 0, logging is turned off, else it's turned on */
void logger(Window *win, char *unused, int flag)
{
	char	*logfile;
	if ((logfile = get_string_var(LOGFILE_VAR)) == NULL)
	{
		say("You must set the LOGFILE variable first!");
		set_int_var(LOG_VAR, 0);
		return;
	}
	irclog_fp = do_log(flag, logfile, irclog_fp);
	if ((irclog_fp == NULL) && flag)
		set_int_var(LOG_VAR, 0);
}

/*
 * set_log_file: sets the log file name.  If logging is on already, this
 * closes the last log file and reopens it with the new name.  This is called
 * automatically when you SET LOGFILE. 
 */
void set_log_file(Window *win, char *filename, int unused)
{
	char	*expanded;

	if (filename)
	{
		if (strcmp(filename, get_string_var(LOGFILE_VAR)))
			expanded = expand_twiddle(filename);
		else
			expanded = expand_twiddle(get_string_var(LOGFILE_VAR));
		set_string_var(LOGFILE_VAR, expanded);
		new_free(&expanded);
		if (irclog_fp)
		{
			logger(curr_scr_win, NULL, 0);
			logger(curr_scr_win, NULL, 1);
		}
	}
}

/*
 * add_to_log: add the given line to the log file.  If no log file is open
 * this function does nothing. 
 */
void add_to_log(FILE *fp, char *line)
{
	if (fp)
	{
		fprintf(fp, "%s\n", !get_int_var(LASTLOG_ANSI_VAR)? stripansicodes(line): line);
		fflush(fp);
	}
}
