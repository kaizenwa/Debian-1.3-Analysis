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

#ifndef lint
static	char	rcsid[] = "@(#)$Id: log.c,v 1.14 1996/03/01 10:37:30 mrg Exp $";
#endif

#include "irc.h"

#include <sys/stat.h>

#include "log.h"
#include "vars.h"
#include "output.h"
#include "ircaux.h"

#if defined(POSIX)
# define fchmod(path, mode) chmod(path, mode)
#endif

FILE	*irclog_fp;

FILE	*
do_log(flag, logfile, fp)
	int	flag;
	char	*logfile;
	FILE	*fp;
{
	time_t	t;

	if (logfile == (char *) 0)
		return ((FILE *) 0);
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
#ifdef NEED_FCHMOD
					chmod(logfile, S_IREAD | S_IWRITE);
#else
#ifndef _IBMR2
					fchmod(fileno(fp),S_IREAD | S_IWRITE);
#else
					int fd = (int) fileno(fp);
					fchmod((char *) &fd, S_IREAD |
							S_IWRITE);
#endif /* !_IBMR2 */
#endif /* M_UNIX */
					fprintf(fp, "IRC log started %.16s\n",
							ctime(&t));
					fflush(fp);
				}
				else
				{
					say("Couldn't open logfile %s: %s",
						logfile, strerror(errno));
					fp = (FILE *) 0;
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
			fp = (FILE *) 0;
			say("Logfile ended");
		}
	}
	return (fp);
}

/* logger: if flag is 0, logging is turned off, else it's turned on */
void
logger(flag)
	int	flag;
{
	char	*logfile;

	if ((logfile = get_string_var(LOGFILE_VAR)) == (char *) 0)
	{
		say("You must set the LOGFILE variable first!");
		set_int_var(LOG_VAR, 0);
		return;
	}
	irclog_fp = do_log(flag, logfile, irclog_fp);
	if ((irclog_fp == (FILE *) 0) && flag)
		set_int_var(LOG_VAR, 0);
}

/*
 * set_log_file: sets the log file name.  If logging is on already, this
 * closes the last log file and reopens it with the new name.  This is called
 * automatically when you SET LOGFILE. 
 */
void
set_log_file(filename)
	char	*filename;
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
			logger(0);
			logger(1);
		}
	}
}

/*
 * add_to_log: add the given line to the log file.  If no log file is open
 * this function does nothing. 
 */
void
add_to_log(fp, line)
	FILE	*fp;
	char	*line;
{
	if (fp)
	{
		fprintf(fp, "%s\n", line);
		fflush(fp);
	}
}
