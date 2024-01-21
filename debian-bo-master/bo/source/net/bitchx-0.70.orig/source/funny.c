/*
 * funny.c: Well, I put some stuff here and called it funny.  So sue me. 
 *
 * written by michael sandrof
 *
 * copyright(c) 1990 
 *
 * see the copyright file, or do a help ircii copyright 
 */


#include "irc.h"

#include "ircaux.h"
#include "hook.h"
#include "vars.h"
#include "funny.h"
#include "names.h"
#include "server.h"
#include "lastlog.h"
#include "ircterm.h"
#include "output.h"
#include "numbers.h"
#include "parse.h"
#include "status.h"
#include "misc.h"
#include "screen.h"

static	char	*match_str = NULL;

static	int	funny_min;
static	int	funny_max;
static	int	funny_flags;

void
funny_match(stuff)
	char	*stuff;
{
	malloc_strcpy(&match_str, stuff);
}

void
set_funny_flags(min, max, flags)
	int	min,
		max,
		flags;
{
	funny_min = min;
	funny_max = max;
	funny_flags = flags;
}

struct	WideListInfoStru
{
	char	*channel;
	int	users;
};

typedef	struct WideListInfoStru WideList;

static	WideList **wide_list = NULL;
static	int	wl_size = 0;
static	int	wl_elements = 0;

static	int	funny_widelist_users _((WideList **, WideList **));
static	int	funny_widelist_names _((WideList **, WideList **));

static	int
funny_widelist_users(left, right)
	WideList	**left,
			**right;
{
	if ((**left).users > (**right).users)
		return -1;
	else if ((**right).users > (**left).users)
		return 1;
	else
		return my_stricmp((**left).channel, (**right).channel);
}

static	int
funny_widelist_names(left, right)
	WideList	**left,
			**right;
{
	int	comp;

	if ((comp = my_stricmp((**left).channel, (**right).channel)) != 0)
		return comp;
	else if ((**left).users > (**right).users)
		return -1;
	else if ((**right).users > (**left).users)
		return 1;
	else
		return 0;
}


void funny_print_widelist()
{
	int	i;
	char	buffer1[BIG_BUFFER_SIZE];
	char	buffer2[BIG_BUFFER_SIZE];
	char	*ptr;

	if (!wide_list)
		return;

	if (funny_flags & FUNNY_NAME)
		qsort((void *) wide_list, wl_elements, sizeof(WideList *),
		(int (*) _((const void *, const void *))) funny_widelist_names);
	else if (funny_flags & FUNNY_USERS)
		qsort((void *) wide_list, wl_elements, sizeof(WideList *),
		(int (*) _((const void *, const void *))) funny_widelist_users);

	*buffer1 = '\0';
	for (i = 1; i < wl_elements; i++)
	{
		sprintf(buffer2, "%s(%d) ", wide_list[i]->channel,
				wide_list[i]->users);
		ptr = index(buffer1, '\0');
		if (strlen(buffer1) + strlen(buffer2) > CO - 5)
		{
			if (do_hook(WIDELIST_LIST, "%s", buffer1))
		put_it("%s", convert_output_format(get_string_var(FORMAT_WIDELIST_VAR), "%s %s", update_clock(GET_TIME), buffer1));
			*buffer1 = '\0';
			strcat(buffer1, buffer2);
		}
		else
			strcpy(ptr, buffer2);
	}
	if (*buffer1 && do_hook(WIDELIST_LIST, "%s", buffer1))
		put_it("%s", convert_output_format(get_string_var(FORMAT_WIDELIST_VAR), "%s %s", update_clock(GET_TIME), buffer1));
	for (i = 0; i < wl_elements; i++)
	{
		new_free(&wide_list[i]->channel);
		new_free((char **)&wide_list[i]);
	}
	new_free((char **)&wide_list);
	wl_elements = wl_size = 0;
}

/*ARGSUSED*/
void
funny_list(from, ArgList)
	char	*from;
	char	**ArgList;
{
	char	*channel,
		*user_cnt,
		*line;
	WideList **new_list;
	int	cnt;
	static	char	format[25];
	static	int	last_width = -1;

	if (last_width != get_int_var(CHANNEL_NAME_WIDTH_VAR))
	{
		if ((last_width = get_int_var(CHANNEL_NAME_WIDTH_VAR)) != 0)
			sprintf(format, "%%s %%-%u.%us %%-5s %%s", /*thing_ansi,*/
				(unsigned char) last_width,
				(unsigned char) last_width);
		else
			sprintf(format, "%%s %%s %%-5s %%s"/*, thing_ansi*/);
	}
	channel = ArgList[0];
	user_cnt = ArgList[1];
	line = PasteArgs(ArgList, 2);
	if (funny_flags & FUNNY_TOPIC && !(line && *line))
			return;
	cnt = my_atol(user_cnt);
	if (funny_min && (cnt < funny_min))
		return;
	if (funny_max && (cnt > funny_max))
		return;
	if ((funny_flags & FUNNY_PRIVATE) && (*channel != '*'))
		return;
	if ((funny_flags & FUNNY_PUBLIC) && (*channel == '*'))
		return;
	if (match_str)
	{
		if (wild_match(match_str, channel) == 0)
			return;
	}
	if (funny_flags & FUNNY_WIDE)
	{
		if (wl_elements >= wl_size)
		{
			new_list = (WideList **) new_malloc(sizeof(WideList *) *
			    (wl_size + 50));
			bzero(new_list, sizeof(WideList *) * (wl_size + 50));
			if (wl_size)
				bcopy(wide_list, new_list, sizeof(WideList *)
					* wl_size);
			wl_size += 50;
			new_free((char **)&wide_list);
			wide_list = new_list;
		}
		wide_list[wl_elements] = (WideList *)
			new_malloc(sizeof(WideList));
		wide_list[wl_elements]->channel = NULL;
		wide_list[wl_elements]->users = cnt;
		malloc_strcpy(&wide_list[wl_elements]->channel,
				(*channel != '*') ? channel : "Prv");
		wl_elements++;
		return;
	}
	if (do_hook(current_numeric, "%s %s %s %s", from,  channel, user_cnt,
	    line) && do_hook(LIST_LIST, "%s %s %s", channel, user_cnt, line))
	{
		if (channel && user_cnt)
			put_it("%s", convert_output_format(get_string_var(FORMAT_LIST_VAR),"%s %s %s %s", update_clock(GET_TIME), *channel == '*'?"Prv":channel, user_cnt, line));
	}
}

void print_funny_names(char *line)
{
char *t;
int count = 0;
char *buffer = NULL;
char special = '\0';
	if (*line)
	{	
		t = next_arg(line, &line);
		if (get_string_var(FORMAT_NAMES_BANNER_VAR))
			malloc_strcpy(&buffer, get_string_var(FORMAT_NAMES_BANNER_VAR));
		do {
			if (*t == '@' || *t == '+' || *t == '~' || *t == '-')
			{
				special = *t;
				if (special == '+')
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_VOICECOLOR_VAR),"%c %s", special, ++t));
				else
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_OPCOLOR_VAR),"%c %s", special, ++t));
			}
			else
				malloc_strcat(&buffer, convert_output_format(get_string_var(FORMAT_NAMES_NICKCOLOR_VAR), "$ %s", t));
			malloc_strcat(&buffer, " ");
			if (count++ == 4)
			{
				put_it(buffer);
				if (get_string_var(FORMAT_NAMES_BANNER_VAR))
					malloc_strcpy(&buffer, get_string_var(FORMAT_NAMES_BANNER_VAR));
				else
					new_free(&buffer);
				count = 0;
			}
		} while ((t = next_arg(line, &line)));

		if (buffer)
			put_it(buffer);
		new_free(&buffer);
	}
}

void
funny_namreply(from, Args)
	char	*from;
	char	**Args;
{
	char	*type,
		*channel;
	static	char	format[40];
	static	int	last_width = -1;
	int	cnt;
	char	*ptr;
	char	*line, *t;
	int user_count = 0;
	PasteArgs(Args, 2);
	type = Args[0];
	channel = Args[1];
	line = Args[2];
	message_from(channel, LOG_CRAP);

	if (in_join_list(channel, from_server))
	{
		for (t = line; *t; t++)
		{
			if (*t == ' ')
				user_count++;
		}
		if (do_hook(current_numeric, "%s %s %s %s", from, type, channel,line) && get_int_var(SHOW_CHANNEL_NAMES_VAR))
		{
			put_it("%s", convert_output_format(get_string_var(FORMAT_NAMES_VAR), "%s %s %d",update_clock(GET_TIME), channel, user_count));
			print_funny_names(line);
		} 

		got_info(channel, from_server, GOTNAMES);
		message_from(NULL, LOG_CURRENT);
		return;
	}
	if (last_width != get_int_var(CHANNEL_NAME_WIDTH_VAR))
	{
		if ((last_width = get_int_var(CHANNEL_NAME_WIDTH_VAR)) != 0)
			sprintf(format, "%%s: %%-%u.%us %%s",
				(unsigned char) last_width,
				(unsigned char) last_width);
		else
			strcpy(format, "%s: %s\t%s");
	}
	ptr = line;
	for (cnt = -1; ptr; cnt++)
	{
		if ((ptr = index(ptr, ' ')) != NULL)
			ptr++;
	}
	if (funny_min && (cnt < funny_min))
		return;
	else if (funny_max && (cnt > funny_max))
		return;
	if ((funny_flags & FUNNY_PRIVATE) && (*type == '='))
		return;
	if ((funny_flags & FUNNY_PUBLIC) && (*type == '*'))
		return;
	if (type && channel)
	{
		if (match_str)
		{
			if (wild_match(match_str, channel) == 0)
				return;
		}
		if (do_hook(current_numeric, "%s %s %s %s", from, type, channel, line) && do_hook(NAMES_LIST, "%s %s", channel, line))
		{
			if (get_string_var(FORMAT_NAMES_VAR))
			{
				put_it("%s", convert_output_format(get_string_var(FORMAT_NAMES_VAR), "%s %s %d", update_clock(GET_TIME), channel, cnt));
				print_funny_names(line);
			} 
			else
			{
				switch (*type)
				{
				case '=':
					if (last_width &&(strlen(channel) > last_width))
					{
						channel[last_width-1] = '>';
						channel[last_width] = (char) 0;
					}
					put_it(format, "Pub", channel, line);
					break;
				case '*':
					put_it(format, "Prv", channel, line);
					break;
				case '@':
					put_it(format, "Sec", channel, line);
					break;
				}
			}
		}
	}
	message_from(NULL, LOG_CURRENT);
}

void
funny_mode(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*mode, *channel;
	ChannelList *chan = NULL;
		
	if (!ArgList[0]) return;
	channel = ArgList[0];
	mode = ArgList[1];
	PasteArgs(ArgList, 1);
	if((channel && in_join_list(channel, from_server)) || get_chan_from_join_list(from_server))
	{
		if (!channel)
			channel = get_chan_from_join_list(from_server);
		update_channel_mode(from, channel, from_server, mode, chan);
		update_all_status(curr_scr_win, NULL, 0);
		got_info(channel, from_server, GOTMODE);
/*		check_sync(324, channel, NULL, NULL, NULL, chan);*/
	}
	else
	{
		if (channel)
		{
			message_from(channel, LOG_CRAP);
			if (do_hook(current_numeric, "%s %s %s", from, channel, mode))
				put_it("%s", convert_output_format(get_string_var(FORMAT_MODE_CHANNEL_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, *FromUserHost ? FromUserHost:"�", channel, mode));
		}
		else
		{
			if (do_hook(current_numeric, "%s %s", from, mode))
				put_it("%s", convert_output_format(get_string_var(FORMAT_MODE_CHANNEL_VAR), "%s %s %s %s %s", update_clock(GET_TIME), from, *FromUserHost ? FromUserHost:"�", "", mode, ""));
		}
	}
}

void
update_user_mode(modes)
	char	*modes;
{
	int	onoff = 1;

	for (; *modes; modes++)
	{
		if (*modes == '-')
			onoff = 0;
		else if (*modes == '+')
			onoff = 1;

		else if   ((*modes >= 'a' && *modes <= 'z')
			|| (*modes >= 'A' && *modes <= 'Z'))
		{
			char c = tolower(*modes);
			size_t idx = (size_t) (index(umodes, c) - umodes);

			set_server_flag(from_server, USER_MODE << idx, onoff);

			if (c == 'o')
				set_server_operator(from_server, onoff);
		}
	}
}

extern void	reinstate_user_modes _((void))
{
	char *modes = get_umode(from_server);
	if (modes && *modes)
		send_to_server("MODE %s +%s", get_server_nickname(from_server), modes);
}
