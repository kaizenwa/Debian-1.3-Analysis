/*
 * output.c: handles a variety of tasks dealing with the output from the irc
 * program 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#include "irc.h"
#include <sys/ioctl.h>
#include <sys/stat.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "output.h"
#include "vars.h"
#include "input.h"
#include "ircterm.h"
#include "lastlog.h"
#include "window.h"
#include "screen.h"
#include "hook.h"
#include "ctcp.h"
#include "log.h"
#include "misc.h"

	int	in_help = 0;

/* make this buffer *much* bigger than needed */
static	char	putbuf[BIG_BUFFER_SIZE * 2 + 1];
extern	LastMsg	last_servermsg[];

char three_stars[4] = "***";

/* unflash: sends a ^[c to the screen */
/* Must be defined to be useful, cause some vt100s really *do* reset when
   sent this command. >;-) */
/* Now that you can send ansi sequences, this is much less inportant.. */
void unflash _((void))
{
#ifdef HARD_UNFLASH
	fwrite("\033c\033(U", 5, 1, stdout);		/* hard reset */
#else
	fwrite("\033)0\033(U", 6, 1, stdout);		/* soft reset */
#endif
}

/*
 * refresh_screen: Whenever the REFRESH_SCREEN function is activated, this
 * swoops into effect 
 */
/* More keybinding lossage -- who the **** left all these old protos around? */

void refresh_screen (unsigned char dumb, char *dumber)
{
	term_clear_screen();
	unflash();
	if (term_resize())
		recalculate_windows();
	else
		redraw_all_windows();
	update_all_windows();
	update_input(UPDATE_ALL);
}

/* init_windows:  */
void	init_screen _((void))
{
	term_init();
	term_clear_screen();
	term_resize();
	new_window();
	recalculate_windows();
	update_all_windows();
	init_input();
	term_move_cursor(0, 0);
}

/* put_file: uses put_it() to display the contents of a file to the display */
#ifdef __STDC__
void put_file (char *filename)
#else
void	put_file(filename)
char	*filename;
#endif
{
	FILE	*fp;
	char	line[256];		/* too big?  too small?  who cares? */
	int	len;

	if ((fp = fopen(filename, "r")) != (FILE *) 0)
	{
		while (fgets(line, 256, fp))
		{
			len = strlen(line);
			if (*(line + len - 1) == '\n')
				*(line + len - 1) = (char) 0;
			put_it("%s", line);
		}
		fclose(fp);
	}
}

/*
 * put_it: the irc display routine.  Use this routine to display anything to
 * the main irc window.  It handles sending text to the display or stdout as
 * needed, add stuff to the lastlog and log file, etc.  Things NOT to do:
 * Dont send any text that contains \n, very unpredictable.  Tabs will also
 * screw things up.  The calling routing is responsible for not overwriting
 * the 1K buffer allocated.  
 *
 * For Ultrix machines, you can't call put_it() with floating point arguements.
 * It just doesn't work.  - phone, jan 1993.
 */
#if defined(__STDC__) && defined(HAVE_STDARG_H)
void put_it(char *format, ...)
#else
void	put_it(format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
char	*format;
char	*arg1, *arg2, *arg3, *arg4, *arg5,
	*arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
	if (window_display && format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		memset(putbuf, 0, 200);
		va_start (args, format);
		vsprintf(putbuf, format, args);
		va_end(args);
#else
		sprintf(putbuf, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
#if 0
		if (strip_ansi_in_echo) {
		register char *ptr;
			for (ptr = putbuf; *ptr; ptr++)
				if (*ptr < 31 && *ptr > 13)
					if (*ptr != 15 && *ptr != 22)
						*ptr = (*ptr & 127) | 64;
		}
#endif
		if (*putbuf)
		{
			add_to_log(irclog_fp, putbuf);
			add_to_screen(putbuf);
		}
	}
}

/* This is an alternative form of put_it which writes three asterisks
 * before actually putting things out.
 */
#if defined(__STDC__) && defined(HAVE_STDARG_H)
void say (char *format, ...)
#else
void	say(format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
char	*format;
char	*arg1, *arg2, *arg3, *arg4, *arg5,
	*arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
int len = 0;
	if (window_display && format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start (args, format);
		if (thing_ansi)
			len = strlen(thing_ansi);
		else
			len = 3;
		vsprintf(&(putbuf[len+1]), format, args);
		va_end(args);
#else
		sprintf(&(putbuf[4]), format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
		strcpy(putbuf, thing_ansi?thing_ansi:three_stars);
		putbuf[len] = ' ';
		if (strip_ansi_in_echo) {
		register char *ptr;
			for (ptr = putbuf + len; *ptr; ptr++)
				if (*ptr < 31 && *ptr > 13)
					if (*ptr != 15 && *ptr != 22)
						*ptr = (*ptr & 127) | 64;
		}
		add_to_log(irclog_fp, putbuf);
		add_to_screen(putbuf);
	}
}

#if defined(__STDC__) && defined(HAVE_STDARG_H)
void bitchsay (char *format, ...)
#else
void	bitchsay(format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
char	*format;
char	*arg1, *arg2, *arg3, *arg4, *arg5,
	*arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
int len;
	if (window_display && format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start (args, format);
		sprintf(putbuf, "%s \002%s\002: ", thing_ansi?thing_ansi:three_stars, version);
		len = strlen(putbuf);
		vsprintf(&(putbuf[len]), format, args);
		va_end(args);
#else
		sprintf(putbuf, "%s \002%s\002: ", thing_ansi?thing_ansi:three_stars, version);
		len = strlen(putbuf);
		sprintf(&(putbuf[len]), format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
		if (strip_ansi_in_echo) {
		register char *ptr;
			for (ptr = putbuf+len; *ptr; ptr++)
				if (*ptr < 31 && *ptr > 13)
					if (*ptr != 15 && *ptr != 22)
						*ptr = (*ptr & 127) | 64;
		}
		add_to_log(irclog_fp, putbuf);
		add_to_screen(putbuf);
	}
}

#if defined(__STDC__) && defined(HAVE_STDARG_H)
void	yell(char *format, ...)
#else
void	yell(format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
char	*format;
char	*arg1, *arg2, *arg3, *arg4, *arg5,
	*arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
	if (format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start (args, format);
		vsprintf(putbuf, format, args);
		va_end(args);
#else
		sprintf(putbuf, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
	}
	add_to_log(irclog_fp, putbuf);
	add_to_screen(putbuf);
}


/* help_put_it: works just like put_it, but is specially used by help */
#if defined(__STDC__) && defined(HAVE_STDARG_H)
void	help_put_it (char *topic, char *format, ...)
#else
void	help_put_it (topic, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
char	*format,
	*topic;
char	*arg1, *arg2, *arg3, *arg4, *arg5,
	*arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
	if (format)
	{
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start (args, format);
		vsprintf(putbuf, format, args);
		va_end(args);
#else
		sprintf(putbuf, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif

		in_help = 1;
		if (do_hook(HELP_LIST, "%s %s", topic, putbuf))
		{
			message_from(NULL, LOG_CURRENT);
			if (window_display)
			{
				add_to_log(irclog_fp, putbuf);
				add_to_screen(convert_output_format(putbuf, NULL, NULL));
			}
			message_from(NULL, LOG_CRAP);
		}
		in_help = 0;
	}
}

#if defined(__STDC__) && defined(HAVE_STDARG_H)
void serversay (int save, char *format, ...)
#else
void    serversay(save, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
int	save;
char    *format;
char    *arg1, *arg2, *arg3, *arg4, *arg5,
        *arg6, *arg7, *arg8, *arg9, *arg10;
#endif
{
	Window	*old_to_window = to_window;
        int     lastlog_level;
        lastlog_level = set_lastlog_msg_level(LOG_CRAP);
	if (get_int_var(OV_VAR))
		to_window = get_window_by_name("OV");
        if (window_display && format)
        {
#if defined(__STDC__) && defined(HAVE_STDARG_H)
		va_list args;
		va_start (args, format);
		vsprintf(&(putbuf[strlen(get_string_var(SERVER_PROMPT_VAR))+1]), format, args);
		va_end(args);
#else

	sprintf(&(putbuf[strlen(putbuf)]), format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
#endif
		strcpy(putbuf, get_string_var(SERVER_PROMPT_VAR));
		putbuf[strlen(get_string_var(SERVER_PROMPT_VAR))] = ' ';

		if (*putbuf)
		{
			add_to_log(irclog_fp, putbuf);
			add_to_screen(putbuf);
		}
	}
	if (get_int_var(OV_VAR))
		to_window = old_to_window;
	set_lastlog_msg_level(lastlog_level);
	if (save)
		add_last_type(&last_servermsg[0], NULL, NULL, NULL, putbuf);
}
