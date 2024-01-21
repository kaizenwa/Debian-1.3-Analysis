/*
 * status.c: handles the status line updating, etc for IRCII 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */


#include "irc.h"

#include "ircterm.h"
#include "status.h"
#include "server.h"
#include "vars.h"
#include "hook.h"
#include "input.h"
#include "edit.h"
#include "window.h"
#include "screen.h"
#include "mail.h"
#include "output.h"
#include "names.h"
#include "ircaux.h"
#include "misc.h"


#define MY_BUFFER 150

extern char *DCC_get_current_transfer _((void));
extern char *ltoa _((long));
extern	int	oper_kills;
extern	int	nick_collisions;

static	char	*convert_format _((Window *, char *, int));
static	char	*status_nickname _((Window *));
static	char	*status_query_nick _((Window *));
static	char	*status_right_justify _((Window *));
static	char	*status_chanop _((Window *));
static	char	*status_channel _((Window *));
static	char	*status_server _((Window *));
static	char	*status_mode _((Window *));
static	char	*status_umode _((Window *));
static	char	*status_insert_mode _((Window *));
static	char	*status_overwrite_mode _((Window *));
static	char	*status_away _((Window *));
static	char	*status_oper _((Window *));
static	char	*status_users _((Window *));
static	char	*status_user0 _((Window *));
static	char	*status_user1 _((Window *));
static	char	*status_user2 _((Window *));
static	char	*status_user3 _((Window *));
static	char	*status_user4 _((Window *));
static	char	*status_user5 _((Window *));
static	char	*status_user6 _((Window *));
static	char	*status_user7 _((Window *));
static	char	*status_user8 _((Window *));
static	char	*status_user9 _((Window *));
static	char	*status_user10 _((Window *));
static	char	*status_user11 _((Window *));
static	char	*status_user12 _((Window *));
static	char	*status_user13 _((Window *));
static	char	*status_user14 _((Window *));
static	char	*status_user15 _((Window *));
static	char	*status_user16 _((Window *));
static	char	*status_user17 _((Window *));
static	char	*status_user18 _((Window *));
static	char	*status_user19 _((Window *));
static	char	*status_lag   _((Window *));
static	char	*status_dcc _((Window *));
static	char	*status_oper_kills _((Window *));
static	char	*status_msgcount _((Window *));
static	char	*status_hold _((Window *));
static	char	*status_version _((Window *));
static	char	*status_clock _((Window *));
static	char	*status_hold_lines _((Window *));
static	char	*status_window _((Window *));
static	char	*status_mail _((Window *));
static	char	*status_refnum _((Window *));
static	char	*status_topic _((Window *));
static	char	*status_null_function _((Window *));
static	char	*status_notify_windows _((Window *));
static	void	status_make_printable _((unsigned char *, int));
static	char	*convert_sub_format _((char *, char));
static	char	*status_voice _((Window *));

#define cparse(format, str) convert_output_format(get_string_var(format), "%s", str)

/*
 * Maximum number of "%" expressions in a status line format.  If you change
 * this number, you must manually change the sprintf() in make_status 
 */
/*#define MAX_FUNCTIONS 36*/

/* The format statements to build each portion of the status line */
#if 0
static	char	*mode_format = NULL;
static	char	*umode_format = NULL;
static	char	*topic_format = NULL;
static	char	*query_format = NULL;
static	char	*clock_format = NULL;
static	char	*hold_lines_format = NULL;
static	char	*channel_format = NULL;
static	char	*mail_format = NULL;
static	char	*server_format = NULL;
static	char	*notify_format = NULL;
static	char	*status_oper_kills_format = NULL;
static	char	*status_users_format = NULL;
static	char	*status_lag_format = NULL;
static	char	*status_format[4] = {NULL, NULL, NULL, NULL};
#endif

/* update_clock: figures out the current time and returns it in a nice format */
char	*
update_clock(flag)
	int	flag;
{
	extern	time_t	start_time;
	static	char	time_str[40];
	static	int	min = -1,
			hour = -1;
	struct tm	*time_val;
	char	*merid;
	time_t	t;

	t = time(NULL);
	time_val = localtime(&t);
	if (flag == RESET_TIME || time_val->tm_min != min || time_val->tm_hour != hour)
	{
		int	tmp_hour,
			tmp_min,
			server;
		time_t idle_t;
		
		tmp_hour = time_val->tm_hour;
		tmp_min = time_val->tm_min;

		if (get_int_var(CLOCK_24HOUR_VAR))
			merid = empty_string;
		else
		{
			if (time_val->tm_hour < 12)
				merid = "am";
			else
				merid = "pm";
			if (time_val->tm_hour > 12)
				time_val->tm_hour -= 12;
			else if (time_val->tm_hour == 0)
				time_val->tm_hour = 12;
		}
		server = from_server;
		from_server = primary_server;
		sprintf(time_str, "%02d:%02d%s", time_val->tm_hour, time_val->tm_min, merid);
		if (tmp_min != min || tmp_hour != hour)
		{
			hour = tmp_hour;
			min = tmp_min;
			do_hook(TIMER_LIST, "%s", time_str);
		}
		idle_t = (t - idle_time) / 60L;
		if ((do_hook(IDLE_LIST, "%ld", idle_t)) && get_int_var(AUTO_AWAY_TIME_VAR))
			if (idle_t >= get_int_var(AUTO_AWAY_TIME_VAR)/60)
				auto_away(idle_t);
		if ((t - start_time) > 20)
			check_serverlag(get_server_itsname(from_server));
		from_server = server;
		return (time_str);
	}
	if (flag == GET_TIME)
		return(time_str);
	else
		return (NULL);
}

/*ARGSUSED*/
void reset_clock(win, unused, unused1)
	Window	*win;
	char	*unused;
	int	unused1;
{
	update_clock(RESET_TIME);
	update_all_status(curr_scr_win, NULL, 0);
}

/*
 * convert_sub_format: This is used to convert the formats of the
 * sub-portions of the status line to a format statement specially designed
 * for that sub-portions.  convert_sub_format looks for a single occurence of
 * %c (where c is passed to the function). When found, it is replaced by "%s"
 * for use is a sprintf.  All other occurences of % followed by any other
 * character are left unchanged.  Only the first occurence of %c is
 * converted, all subsequence occurences are left unchanged.  This routine
 * mallocs the returned string. 
 */
static	char	*
#ifdef __STDC__
convert_sub_format(char *format, char c)
#else
convert_sub_format(format, c)
	char	*format;
	char	c;
#endif
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	static	char	bletch[] = "%% ";
	char	*ptr = NULL;
	int	dont_got_it = 1;
	
	if (format == NULL)
		return (NULL);
	*buffer = (char) 0;
	memset(buffer, 0, sizeof(buffer));
	while (format)
	{
		if ((ptr = (char *) index(format, '%')) != NULL)
		{
			*ptr = (char) 0;
			strmcat(buffer, format, BIG_BUFFER_SIZE);
			*(ptr++) = '%';
			if ((*ptr == c)/* && dont_got_it*/)
			{
				dont_got_it = 0;
				strmcat(buffer, "%s", BIG_BUFFER_SIZE);
			}
			else
			{
				bletch[2] = *ptr;
				strmcat(buffer, bletch, BIG_BUFFER_SIZE);
			}
			ptr++;
		}
		else
			strmcat(buffer, format, BIG_BUFFER_SIZE);
		format = ptr;
	}
	malloc_strcpy(&ptr, buffer);
	return (ptr);
}

static	char	*convert_format(Window *win, char *format, int k)
{
	char	buffer[BIG_BUFFER_SIZE + 1];
	char	*ptr,
		*malloc_ptr = NULL; 
	int	*cp;
	
	bzero(buffer, sizeof(buffer));
	while (format)
	{
		if ((ptr = (char *) index(format, '%')) != NULL)
		{
			int	extended = 0;
			char	*nextspot = NULL;

			*ptr = (char) 0;
			strmcat(buffer, format, BIG_BUFFER_SIZE);
			*(ptr++) = '%';

			cp = &win->func_cnt[k];

			if (*cp < MAX_FUNCTIONS)
			{
				if (*ptr == '!') {
					extended = 1;
					/* get over the '!' char */
					ptr++;
					nextspot = ptr + 1;
				}
				switch (*(ptr++))
				{
				case '%':
					strmcat(buffer, "%", BIG_BUFFER_SIZE);
					break;
				case '>':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_right_justify;
					break;

				case '^':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_msgcount;
					break;

				case '@':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_chanop;
					break;

				case '+':
					new_free(&win->mode_format);
					win->mode_format =
		convert_sub_format(wget_string_var(win, STATUS_MODE_VAR), '+');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_mode;
					break;
				case '-':
					new_free(&win->topic_format);
					win->topic_format =
		convert_sub_format(wget_string_var(win, STATUS_TOPIC_VAR), '-');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_topic;
					break;
				case '#':
					new_free(&win->umode_format);
					win->umode_format = 
		convert_sub_format(wget_string_var(win, STATUS_UMODE_VAR), '#');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_umode;
					break;
				case '=':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_voice;
					break;
				case '*':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_oper;
					break;

				case 'A':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_away;
					break;
				case 'B':
					new_free(&win->hold_lines_format);
					win->hold_lines_format =
		convert_sub_format(wget_string_var(win, STATUS_HOLD_LINES_VAR), 'B');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] =
						status_hold_lines;
					break;
				case 'C':
					new_free(&win->channel_format);
					win->channel_format =
		convert_sub_format(wget_string_var(win, STATUS_CHANNEL_VAR), 'C');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_channel;
					break;
				case 'D':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_dcc;
					break;
				case 'E':
					break;
				case 'F':
					new_free(&win->notify_format);
					win->notify_format = 
		convert_sub_format(wget_string_var(win, STATUS_NOTIFY_VAR), 'F');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_notify_windows;
					break;
				case 'G':
					break;
				case 'H':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_hold;
					break;
				case 'I':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_insert_mode;
					break;
				case 'J':
					break; 
				case 'K':
					new_free(&win->status_oper_kills_format);
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_oper_kills_format =
		convert_sub_format(wget_string_var(win, STATUS_OPER_KILLS_VAR), 'K');
					win->status_func[k][(*cp)++] = status_oper_kills;
					break; 
				case 'L':
					new_free(&win->status_lag_format);
					win->status_lag_format = 
		convert_sub_format(wget_string_var(win, STATUS_LAG_VAR), 'L');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_lag;
					break; 
				case 'M':
					new_free(&win->mail_format);
					win->mail_format =
		convert_sub_format(wget_string_var(win, STATUS_MAIL_VAR), 'M');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_mail;
					break;
				case 'N':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_nickname;
					break;
				case 'O':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_overwrite_mode;
					break;
				case 'P':
					break;
				case 'Q':
					new_free(&win->query_format);
					win->query_format =
		convert_sub_format(wget_string_var(win, STATUS_QUERY_VAR), 'Q');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] =
						status_query_nick;
					break;
				case 'R':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_refnum;
					break;
				case 'S':
					new_free(&win->server_format);
					win->server_format =
		convert_sub_format(wget_string_var(win, STATUS_SERVER_VAR), 'S');
					strmcat(buffer,"%s",BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_server;
					break;
				case 'T':
					new_free(&win->clock_format);
					win->clock_format =
		convert_sub_format(wget_string_var(win, STATUS_CLOCK_VAR), 'T');
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_clock;
					break;
				case 'U':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user0;
					break;
				case 'V':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_version;
					break;
				case 'W':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_window;
					break;
				case 'X':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user1;
					break;
				case 'Y':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user2;
					break;
				case 'Z':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user3;
					break;
/* Replaced status format % chars with 1-9 instead of XYZ CDE */
				case '1':
					if (extended) {
						switch (*ptr) 
						{
							case '0':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user10;
								break;
							case '1':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user11;
								break;
							case '2':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user12;
								break;
							case '3':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user13;
								break;
							case '4':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user14;
								break;
							case '5':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user15;
								break;
							case '6':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user16;
								break;
							case '7':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user17;
								break;
							case '8':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user18;
								break;
							case '9':
								strmcat(buffer, "%s", BIG_BUFFER_SIZE);
								win->status_func[k][(*cp)++] = status_user19;
								break;
							default:
								/*  empty  */
								break;
						}
						ptr++;
						extended = 0;
					}
					else 
					{
						strmcat(buffer, "%s", BIG_BUFFER_SIZE);
						win->status_func[k][(*cp)++] = status_user1;
					}
					break;
				case '2':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user2;
					break;
				case '3':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user3;
					break;
				case '4':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user4;
					break;
				case '5':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user5;
					break;
				case '6':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user6;
					break;
				case '7':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user7;
					break;
				case '8':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user8;
					break;
				case '9':
					strmcat(buffer, "%s", BIG_BUFFER_SIZE);
					win->status_func[k][(*cp)++] = status_user9;
					break;
		/* no default..?? - phone, jan 1993 */
		/* empty is a good default -lynx, mar 93 */
				}
			}
			else
				ptr++;
			if (extended)
			{
				new_free(&win->status_users_format);
				win->status_users_format = 
				convert_sub_format(wget_string_var(win, STATUS_USERS_VAR), '!');
				strmcat(buffer, "%s", BIG_BUFFER_SIZE);
				win->status_func[k][(*cp)++] = status_users;
	
			}
		}
		else
			strmcat(buffer, format, BIG_BUFFER_SIZE);
		format = ptr;
	}
	/* this frees the old str first */
	malloc_strcpy(&malloc_ptr, buffer);
	return (malloc_ptr);
}

void build_status(Window *win, char *format, int unused)
{
	int	i, k;
	if (!win)
		return;
	for (k = 0; k < 3; k++) {
		new_free(&(win->status_format[k]));
		win->func_cnt[k] = 0;
		for (i = 0; i < MAX_FUNCTIONS; i++)
			win->status_func[k][i] = status_null_function;
		switch(k)
		{
			case 0:
				format = wget_string_var(win, STATUS_FORMAT1_VAR);
				break;
			case 1:
				format = wget_string_var(win, STATUS_FORMAT2_VAR);
				break;
			case 2:
				format = wget_string_var(win, STATUS_FORMAT3_VAR);
				break;
		}			
		if (format != NULL)
		{
			win->status_format[k] = convert_format(win, format, k);
/*			malloc_strcpy(&win->format_status[k], format);*/
		}
		for (i = win->func_cnt[k]; i < MAX_FUNCTIONS; i++)
			win->status_func[k][i] = status_null_function;
	}
	update_all_status(win, NULL, 0);
}

void make_status(Window *win)
{
extern int count_ansi _((char *, int));

	u_char	buffer[3*BIG_BUFFER_SIZE + 1];
	char	*func_value[MAX_FUNCTIONS+10] = {NULL};
	char	tmp[3*BIG_BUFFER_SIZE + 1];

	register int	i;
	int len = 1,
	k,

	RJustifyPos = -1,
	RealPosition = 0;
	
	int no_change = 1;
	
	bzero(tmp, sizeof(tmp));
	bzero(buffer, sizeof(buffer));

	/* The second status line is only displayed in the bottom window
	 * and should always be displayed, no matter what SHOW_STATUS_ALL
	 * is set to - krys
	 */
	for (k = 0 ; k < 1+win->double_status + win->menu.lines; k++)
	{
		int l = k;

		if (!dumb && win->status_format[l])
		{
			for (i = 0; i < MAX_FUNCTIONS; i++)
				func_value[i] = (win->status_func[l][i]) (win);
			len = 1;

    			buffer[0] = get_int_var(REVERSE_STATUS_VAR) ? REV_TOG : ALL_OFF;

			sprintf((char *) buffer+len, win->status_format[l],
				func_value[0], func_value[1], func_value[2],
				func_value[3], func_value[4], func_value[5],
				func_value[6], func_value[7], func_value[8],
				func_value[9], func_value[10], func_value[11],
				func_value[12], func_value[13], func_value[14],
				func_value[15], func_value[16], func_value[17],
				func_value[18], func_value[19], func_value[20],
				func_value[21], func_value[22], func_value[23],
				func_value[24], func_value[25], func_value[26],
				func_value[27], func_value[28], func_value[29],
				func_value[30], func_value[31],func_value[32],
				func_value[33], func_value[34],func_value[35],
				func_value[36], func_value[37],func_value[38]);

			/*  Patched 26-Mar-93 by Aiken
			 *  make_window now right-justifies everything 
			 *  after a %>
			 *  it's also more efficient.
			 */

			RealPosition = 0;
			RJustifyPos = -1;
			for (i = 0; buffer[i] != 0; i++)
				/* formfeed is a marker for left/right border*/
				if (buffer[i] == '\f')
					RJustifyPos = i;
				else if (buffer[i] != REV_TOG
					 && buffer[i] != UND_TOG 
					 && buffer[i] != ALL_OFF 
					 && buffer[i] != BOLD_TOG
					 && !vt100_decode(buffer[i])
					 )
				{
					if (RealPosition == CO - 1)
					{
						buffer[i] = '\0';
						break;
					}
					RealPosition++;
				}

			/* note that i points to the nul, 
			   RealPosition is vis.chars */
			
			if (RJustifyPos == -1)
			{
				RJustifyPos = i;
			}
			else
			{
				/* get rid of the marker */
				if (index(buffer,'\f'))
				{
					strcpy(&buffer[RJustifyPos], &buffer[RJustifyPos+1]);
					i--;
				}
			}
			
			if (get_int_var(FULL_STATUS_LINE_VAR))
			{
				int	diff;
				u_char	c;
				
				if (RJustifyPos == 0)
					c = ' ';
				else
					c = buffer[RJustifyPos - 1];
				
				diff = CO - RealPosition;
				
				for ( ; i >= RJustifyPos; i--)
					buffer[i + diff] = buffer[i];
				
				for (i++ ; diff > 0 ; diff--, i++)
					buffer[i] = c;
			}
			
			if (get_int_var(DISPLAY_ANSI_VAR))
		                strcat(buffer,"[0m");
			len = strlen((char *) buffer);
			buffer[len] = ALL_OFF;
			buffer[len+1] =  '\0';

			status_make_printable(buffer, len);
			
	      /*
	       * Thanks to Max Bell (mbell@cie.uoregon.edu) for info about TVI
	       * terminals and the sg terminal capability 
	       */
			RealPosition = 0;
#ifdef WINNT
			if (!get_int_var(DISPLAY_ANSI_VAR) && win->status_line[k]/* && (SG == -1)*/)
#else
			if (!get_int_var(DISPLAY_ANSI_VAR) && win->status_line[k] && (SG == -1))
#endif
			{
				for (i = 0; buffer[i] && win->status_line[k][i]; i++)
				{
					if ((char) buffer[i] != (u_char) win->status_line[k][i])
						break;
					if (buffer[i] != REV_TOG 
					    && buffer[i] != UND_TOG
					    && buffer[i] != ALL_OFF
					    && buffer[i] != BOLD_TOG)
						RealPosition++;
				}
			}
			else
			{
				/* Thanks to cyberd0g for finding this */
				if (win->status_line[k])
					no_change = strcmp(buffer, win->status_line[k]);
				i = 0;
			}			

			if (no_change && ((len = strlen((char *) buffer + i)) 
			     || buffer[i] || win->status_line[k] 
			     || win->status_line[k][i]))
			{
				Screen *old_current_screen;
				old_current_screen = current_screen;
				set_current_screen(win->screen);
				if (win->menu.lines && (l == win->double_status+win->menu.lines))
				{
					term_move_cursor(RealPosition,win->top);
					if (get_int_var(DISPLAY_ANSI_VAR))
						output_line(cparse(FORMAT_STATUS3_VAR, buffer), NULL, i);
					else
						output_line(buffer, NULL, i);
				}
				else
				{
					term_move_cursor(RealPosition,win->bottom+k);
	
					if (get_int_var(DISPLAY_ANSI_VAR))
						output_line(cparse((l==3)?FORMAT_STATUS3_VAR:(l==2)?FORMAT_STATUS2_VAR:(l==1)?FORMAT_STATUS1_VAR:FORMAT_STATUS_VAR, buffer), NULL, i);
					else
						output_line(buffer, NULL, i);
				}
				cursor_in_display();
				term_clear_to_eol();
				malloc_strcpy(&win->status_line[k], (char *) buffer);
				set_current_screen(old_current_screen);
			}
		}
	}
	cursor_to_input();
}

static	char	*
status_nickname(window)
	Window	*window;
{

	if ((connected_to_server) && !get_int_var(SHOW_STATUS_ALL_VAR)
	    && (!window->current_channel) &&
	    (window->screen->current_window != window))
		return empty_string;
	else
		return (get_server_nickname(window->server));
}

static	char	*
status_server(window)
	Window	*window;
{
	char	*rest,
		*name;
static	char	my_buffer[MY_BUFFER+1] = "\0";
	if (connected_to_server)
	{
		if (window->server != -1)
		{
			if (window->server_format)
			{
				name = get_server_name(window->server);
				if ((rest = (char *) index(name, '.')) != NULL)
				{
					if (rest - name > 31)
						rest = name + 31;
					if (is_number(name))
						sprintf(my_buffer, window->server_format,
							name);
					else
					{
						*rest = '\0';
						sprintf(my_buffer, window->server_format,
							name);
						*rest = '.';
					}
				}
				else
					sprintf(my_buffer, window->server_format, name);
			}
			else
				*my_buffer = 0;
		}
		else
			strcpy(my_buffer, " No Server");
	}
	else
		*my_buffer = 0;
	return (my_buffer);
}

static	char	*
status_query_nick(window)
	Window	*window;
{
static	char my_buffer[MY_BUFFER+1] = "\0";

	if (window->query_nick && window->query_format)
	{
		sprintf(my_buffer, window->query_format, window->query_nick);
		return my_buffer;
	}
	else
	return (empty_string);
}

static	char	*
status_right_justify(window)
	Window	*window;
{
static	char	my_buffer[] = "\f";

	return (my_buffer);
}

static	char	*
status_notify_windows(window)
	Window	*window;
{
	int	doneone = 0;
	int	flag = 1;
	char	buf2[MY_BUFFER+2];
static	char	my_buffer[MY_BUFFER+1] = "\0";
	Window *old_window;
	
	if (get_int_var(SHOW_STATUS_ALL_VAR) ||
	    window == window->screen->current_window)
	{
		old_window = window;
		*buf2='\0';
		while ((window = traverse_all_windows(&flag)) != NULL)
		{
			if (window->miscflags & WINDOW_NOTIFIED)
			{
				if (doneone++)
					strmcat(buf2, ",", MY_BUFFER);
				strmcat(buf2, ltoa(window->refnum), MY_BUFFER);
			}
		}
		window = old_window;
	}
	if (doneone && window->notify_format)
	{
		sprintf(my_buffer, window->notify_format, buf2);
		return (my_buffer);
	}
	return empty_string;
}

static	char	*
status_clock(window)
	Window	*window;
{
static	char	my_buf[MY_BUFFER+1] = "\0";

	if ((get_int_var(CLOCK_VAR) && window->clock_format)  &&
	    (get_int_var(SHOW_STATUS_ALL_VAR) ||
	    (window == window->screen->current_window)))
		sprintf(my_buf, window->clock_format, update_clock(GET_TIME));
	else
		*my_buf = 0;
	return (my_buf);
}

static	char	*
status_mode(window)
	Window	*window;
{
char		*mode = NULL;
static  char	my_buffer[MY_BUFFER+1] = "\0";
	if (window->current_channel)
	{
		mode = get_channel_mode(window->current_channel,window->server);
		if (mode && *mode && window->mode_format)
			sprintf(my_buffer, window->mode_format, mode);
		else
			*my_buffer = 0;
	} else
		*my_buffer = 0;
	return (my_buffer);
}


static	char	*
status_umode(window)
	Window	*window;
{
	char	localbuf[MY_BUFFER+1];
static char my_buffer[MY_BUFFER+1] = "\0";

	if ((connected_to_server) && !get_int_var(SHOW_STATUS_ALL_VAR)
	    && (window->screen->current_window != window))
		*localbuf = 0;
	else {
		if (window->server >= 0)
			strmcpy(localbuf, get_umode(window->server), MY_BUFFER);
		else
			*localbuf = 0;
	}
	
	if (*localbuf != '\0' && window->umode_format)
		sprintf(my_buffer, window->umode_format, localbuf);
	else
		*my_buffer = 0;
	return (my_buffer);
}

static	char	*
status_chanop(window)
	Window	*window;
{
	char	*text;
	if (window->current_channel && get_channel_oper(window->current_channel, window->server) &&   
			(text = get_string_var(STATUS_CHANOP_VAR)))
		return (text);
	else
		return (empty_string);
}


static	char	*
status_hold_lines(window)
	Window	*window;
{
	int	num;
static  char	my_buffer[MY_BUFFER+1] = "\0";
	
	if ((num = window->held_lines - window->held_lines%10))
	{
		sprintf(my_buffer, window->hold_lines_format, ltoa(num));
		return(my_buffer);
	}
	else
		return (empty_string);
}

static	char	*
status_msgcount(window)
	Window	*window;
{
static  char	my_buffer[MY_BUFFER+1] = "\0";
	
	sprintf(my_buffer, "[%d]", get_int_var(MSGCOUNT_VAR));
	return(my_buffer);
}

static	char	*
status_channel(window)
	Window	*window;
{
	char	channel[IRCD_BUFFER_SIZE + 1];
static	char	my_buffer[IRCD_BUFFER_SIZE + 1] = "\0";

	if (window->current_channel/* && chan_is_connected(s, window->server)*/)
	{
		int num;
		if (get_int_var(HIDE_PRIVATE_CHANNELS_VAR) &&
		    is_channel_mode(window->current_channel,
				MODE_PRIVATE | MODE_SECRET,
				window->server))
			strmcpy(channel, "*private*", IRCD_BUFFER_SIZE);
		else
			strmcpy(channel, window->current_channel, IRCD_BUFFER_SIZE);

		if ((num = get_int_var(CHANNEL_NAME_WIDTH_VAR)) &&
		    ((int) strlen(channel) > num))
			channel[num] = (char) 0;
		sprintf(my_buffer, window->channel_format, channel);
		return(my_buffer);
	}
	return (empty_string);
}

static	char	*
status_mail(window)
	Window	*window;
{
	char	*number;
static	char	my_buffer[MY_BUFFER+1] = "\0";
	if (window && (get_int_var(MAIL_VAR) && (number = check_mail()) && window->mail_format) &&
	    (get_int_var(SHOW_STATUS_ALL_VAR) ||
	    (window == window->screen->current_window)))
	{
		sprintf(my_buffer, window->mail_format, number);
		return my_buffer;
	}
	return (empty_string);
}

static	char	*
status_insert_mode(window)
	Window	*window;
{
char	*text;

	if (get_int_var(INSERT_MODE_VAR) && (get_int_var(SHOW_STATUS_ALL_VAR)
	    || (window->screen->current_window == window)))
		if ((text = get_string_var(STATUS_INSERT_VAR)))
			return text;
	return (empty_string);
}

static	char	*
status_overwrite_mode(window)
	Window	*window;
{
char	*text;

	if (!get_int_var(INSERT_MODE_VAR) && (get_int_var(SHOW_STATUS_ALL_VAR)
	    || (window->screen->current_window == window)))
	{
	    if ((text = get_string_var(STATUS_OVERWRITE_VAR)))
		return text;
	}
	return (empty_string);
}

static	char	*
status_away(window)
	Window	*window;
{
char	*text;

	if (window && connected_to_server && !get_int_var(SHOW_STATUS_ALL_VAR)
	    && (window->screen->current_window != window))
		return empty_string;
	else if (window)
	{
		if (window->server != -1 && server_list[window->server].away &&
				(text = get_string_var(STATUS_AWAY_VAR)))
			return text;
		else
			return empty_string;
	}
	return empty_string;
}

static	char	*
status_user0(window)
	Window	*window;
{
char	*text;

	if ((text = get_string_var(STATUS_USER_VAR)) &&
	    (get_int_var(SHOW_STATUS_ALL_VAR) ||
	    (window == window->screen->current_window)))
		return (text);
	return (empty_string);
}

static  char    *
status_user1(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER1_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user2(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER2_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return (text);
	return(empty_string);
}

static  char    *
status_user3(window)
	Window  *window;
{
char         *text;

        if ((text = get_string_var(STATUS_USER3_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
       return(empty_string);
}

static  char    *
status_user4(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER4_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user5(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER5_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user6(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER6_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}


static  char    *
status_user7(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER7_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}


static  char    *
status_user8(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER8_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}


static  char    *
status_user9(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER9_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user10(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER10_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user11(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER11_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user12(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER12_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user13(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER13_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user14(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER14_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user15(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER15_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user16(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER16_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user17(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER17_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user18(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER18_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}

static  char    *
status_user19(window)
	Window  *window;
{
char        *text;

        if ((text = get_string_var(STATUS_USER19_VAR)) &&
            (get_int_var(SHOW_STATUS_ALL_VAR) ||
            (window == window->screen->current_window)))
                return(text);
        return (empty_string);
}


static	char	*
status_hold(window)
	Window	*window;
{
char	*text;

	if (window->held && (text = get_string_var(STATUS_HOLD_VAR)))
		return(text);
	return (empty_string);
}

static	char	*status_lag (Window *window)
{
static  char	my_buffer[MY_BUFFER+1] = "\0";
	char	*p = NULL;
	if ((window->server > -1) && window->status_lag_format)
	{
		if (get_server_lag(window->server) > -1)
		{
			p = m_sprintf("%2d",get_server_lag(window->server)); 
			sprintf(my_buffer,window->status_lag_format, p);
			new_free(&p);
		}
		else
			sprintf(my_buffer, window->status_lag_format, "??");
		return(my_buffer);
	}
	return empty_string;
}

static	char	*status_topic (Window *window)
{
static  char	my_buffer[MY_BUFFER+1] = "\0";
char	tmp_buffer[MY_BUFFER+1];
ChannelList *chan;
	if (window && window->current_channel && window->topic_format)
	{
		if ((chan = lookup_channel(window->current_channel, window->server, 0)))
		{
			strmcpy(tmp_buffer, chan->topic?chan->topic: "No Topic", MY_BUFFER-strlen(window->topic_format));
			sprintf(my_buffer, window->topic_format, stripansicodes(tmp_buffer));
			my_buffer[79] = 0;
			return(my_buffer);
		}
	}
	return empty_string;
}

static	char	*
status_oper(window)
	Window	*window;
{
char	*text;

	if (get_server_operator(window->server) &&
			(text = get_string_var(STATUS_OPER_VAR)) &&
			(get_int_var(SHOW_STATUS_ALL_VAR) ||
			connected_to_server || 
			(window->screen->current_window == window)))
		return(text);
	return (empty_string);
}

static	char	*
status_voice(window)
	Window	*window;
{
char	*text;
	if (window->current_channel &&
	    get_channel_voice(window->current_channel, window->server) &&
	    !get_channel_oper(window->current_channel, window->server) &&
	    (text = get_string_var(STATUS_VOICE_VAR)))
		return text;
	return empty_string;
}

static	char	*
status_window(window)
	Window	*window;
{
char	*text;

	if ((text = get_string_var(STATUS_WINDOW_VAR)) &&
	    (number_of_windows() > 1) && (window->screen->current_window == window))
		return(text);
	return (empty_string);
}

static	char	*
status_refnum(window)
	Window	*window;
{
static char my_buffer[MY_BUFFER+1];
	strcpy(my_buffer, window->name ? window->name : ltoa(window->refnum));
	return (my_buffer);
}

static	char	*
status_version(window)
	Window	*window;
{
	if ((connected_to_server) && !get_int_var(SHOW_STATUS_ALL_VAR)
	    && (window->screen->current_window != window))
		return(empty_string);
	return ((char *)irc_version);
}

static	char	* status_oper_kills (Window *window)
{
static char my_buffer[MY_BUFFER+1];
	if (window->status_oper_kills_format)
	{
		sprintf(my_buffer, window->status_oper_kills_format, ltoa(nick_collisions), ltoa(oper_kills));
		return my_buffer;
	}
	return empty_string;	
}

static	char	* status_users (Window *window)
{
static char my_buffer[200];
ChannelList *chan;
NickList *nick;
int serv = window->server;
	if (window->server > -1 && window->status_users_format)
	{
		if ((chan = prepare_command(&serv, NULL, 3)))
		{
			int ops = 0, nonops = 0, voice = 0, ircop = 0, friends = 0;
			char buff[40], buff1[40], buff2[40], buff3[40], buff4[40];
			 
			for (nick = chan->nicks; nick; nick = nick->next)
			{
				if (nick->chanop)
					ops++;
				else
					nonops++;
				if (nick->voice)
					voice++;
				if (nick->ircop)
					ircop++;
				if (nick->userlist)
					friends++;
			}
			strcpy(buff, ltoa(ops)); 
			strcpy(buff1, ltoa(nonops));
			strcpy(buff2,ltoa(ircop)); 
			strcpy(buff3, ltoa(voice));
			strcpy(buff4, ltoa(friends));
			sprintf(my_buffer, window->status_users_format, buff, buff1, buff2, buff3, buff4);
			return my_buffer;
		}
	}
	return empty_string;	
}

static	char	*
status_null_function(window)
	Window	*window;
{
	return (empty_string);
}


static char *status_dcc(Window *window)
{
	return(DCC_get_current_transfer());
}

/*
 * pass an already allocated char array with n bits, and this
 * gets rid of nasty unprintables.
 */
static	void
status_make_printable(str, n)
	unsigned char	*str;
	int n;
{
	unsigned char	*s;
	int	pos;
	char	buffer[BIG_BUFFER_SIZE + 1];

	if (!str || !*str)
		return;

	bzero(buffer, BIG_BUFFER_SIZE);
	for (pos = 0, s = str; s && pos < BIG_BUFFER_SIZE && pos < n; s++)
	{
                if (vt100_decode(*s)) 
                {
			buffer[pos++]=*s;
			continue;
                }
		if (*s < 32)
		{
			switch(*s)
			{
				case UND_TOG:
				case ALL_OFF:
				case REV_TOG:
				case BOLD_TOG:
					buffer[pos++] = *s;
					break;
				default:
					buffer[pos++] = REV_TOG;
					buffer[pos++] = (*s & 0x7f) | 0x40; 
					buffer[pos++] = REV_TOG;
					break;
			}
		}
		else if ((u_char) 0x7f == *s)
		{
			buffer[pos++] = REV_TOG;
			buffer[pos++] = '?';
			buffer[pos++] = REV_TOG;
		}
		else
			buffer[pos++] = *s;
	}
	buffer[pos] = '\0';
	strncpy((char *) str, buffer, pos);
}
