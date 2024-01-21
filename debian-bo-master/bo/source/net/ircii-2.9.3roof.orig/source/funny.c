/*
 * funny.c: Well, I put some stuff here and called it funny.  So sue me. 
 *
 * written by michael sandrof
 *
 * copyright(c) 1990 
 *
 * see the copyright file, or do a help ircii copyright 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: funny.c,v 1.16.2.2 1996/07/20 19:14:37 mrg Exp $";
#endif

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

static	char	*match_str = (char *) 0;

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

static	WideList **wide_list = (WideList **) 0;
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


void
funny_print_widelist()
{
	int	i;
	char	buffer1[BIG_BUFFER_SIZE];
	char	buffer2[BIG_BUFFER_SIZE];
	char	*ptr;

	if (!wide_list)
		return;

	if (funny_flags & FUNNY_NAME)
		qsort((void *) wide_list, wl_elements, sizeof(WideList *),
			(int (*)()) funny_widelist_names);
	else if (funny_flags & FUNNY_USERS)
		qsort((void *) wide_list, wl_elements, sizeof(WideList *),
			(int (*)()) funny_widelist_users);

	*buffer1 = '\0';
	for (i = 0; i < wl_elements; i++)
	{
		sprintf(buffer2, "%s(%d) ", wide_list[i]->channel,
				wide_list[i]->users);
		ptr = index(buffer1, '\0');
		if ((int) strlen(buffer1) + (int) strlen(buffer2) > CO - 5)
		{
			if (do_hook(WIDELIST_LIST, "%s", buffer1))
				say("%s", buffer1);
			*buffer1 = '\0';
			strcat(buffer1, buffer2);
		}
		else
			strcpy(ptr, buffer2);
	}
	if (*buffer1 && do_hook(WIDELIST_LIST, "%s", buffer1))
		say("%s" , buffer1);
	for (i = 0; i < wl_elements; i++)
	{
		new_free(&wide_list[i]->channel);
		new_free(&wide_list[i]);
	}
	new_free(&wide_list);
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
	static	unsigned int	last_width = -1;

	if (last_width != get_int_var(CHANNEL_NAME_WIDTH_VAR))
	{
		if ((last_width = get_int_var(CHANNEL_NAME_WIDTH_VAR)) != 0)
			sprintf(format, "*** %%-%u.%us %%-5s  %%s",
				(unsigned char) last_width,
				(unsigned char) last_width);
		else
			strcpy(format, "*** %s\t%-5s  %s");
	}
	channel = ArgList[0];
	user_cnt = ArgList[1];
	line = PasteArgs(ArgList, 2);
	if (funny_flags & FUNNY_TOPIC && !(line && *line))
			return;
	cnt = atoi(user_cnt);
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
			new_free(&wide_list);
			wide_list = new_list;
		}
		wide_list[wl_elements] = (WideList *)
			new_malloc(sizeof(WideList));
		wide_list[wl_elements]->channel = (char *) 0;
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
		{
			if (*channel == '*')
				put_it(format, "Prv", user_cnt, line);
			else
				put_it(format, channel, user_cnt, line);
		}
	}
}

void
funny_namreply(from, Args)
	char	*from;
	char	**Args;
{
	char	*type,
		*nick,
		*channel;
	static	char	format[40];
	static	unsigned int	last_width = -1;
	int	cnt;
	char	*ptr;
	char	*line;

	PasteArgs(Args, 2);
	type = Args[0];
	channel = Args[1];
	line = Args[2];
	message_from(channel, LOG_CRAP);
	if (in_join_list(channel, parsing_server_index))
	{
		if (do_hook(current_numeric, "%s %s %s %s", from, type, channel,
			line) && get_int_var(SHOW_CHANNEL_NAMES_VAR))
			say("Users on %s: %s", channel, line);
		while ((nick = next_arg(line, &line)) != NULL)
			add_to_channel(channel, nick, parsing_server_index, 0, 0);
		got_info(channel, parsing_server_index, GOTNAMES);
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
		if (do_hook(current_numeric, "%s %s %s %s", from, type, channel,
			line) && do_hook(NAMES_LIST, "%s %s", channel, line))
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
	message_from(NULL, LOG_CURRENT);
}

void
funny_mode(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*mode, *channel;

	if (!ArgList[0])
		return;
	if (get_server_version(parsing_server_index) < Server2_6)
	{
		channel = (char *) 0;
		mode = ArgList[0];
		PasteArgs(ArgList, 0);
	}
	else
	{
		channel = ArgList[0];
		mode = ArgList[1];
		PasteArgs(ArgList, 1);
	}
	/* if (ignore_mode) */
	if((channel && in_join_list(channel, parsing_server_index)) ||
		get_chan_from_join_list(parsing_server_index))
	{
		if (!channel)
			channel = get_chan_from_join_list(parsing_server_index);
		update_channel_mode(channel, parsing_server_index, mode);
		got_info(channel, parsing_server_index, GOTMODE);
		update_all_status();
	}
	else
	{
		if (channel)
		{
			message_from(channel, LOG_CRAP);
			if (do_hook(current_numeric, "%s %s %s", from,
					channel, mode))
				put_it("%s Mode for channel %s is \"%s\"",
					numeric_banner(), channel, mode);
			message_from((char *) 0, LOG_CURRENT);
		}
		else
		{
			if (do_hook(current_numeric, "%s %s", from, mode))
				put_it("%s Channel mode is \"%s\"",
					numeric_banner(), mode);
		}
	}
}

void
update_user_mode(modes)
	char	*modes;
{
	int	onoff = 1;

	while (*modes)
	{
		switch(*modes++)
		{
		case '-':
			onoff=0;
			break;
		case '+':
			onoff=1;
			break;
		case 'o':
		case 'O':
			set_server_operator(parsing_server_index, onoff);
			break;
		case 's':
		case 'S':
			set_server_flag(parsing_server_index, USER_MODE_S, onoff);
			break;
		case 'i':
		case 'I':
			set_server_flag(parsing_server_index, USER_MODE_I, onoff);
			break;
		case 'w':
		case 'W':
			set_server_flag(parsing_server_index, USER_MODE_W, onoff);
			break;
		case 'r':
		case 'R':
			set_server_flag(parsing_server_index, USER_MODE_R, onoff);
			break;
		}
	}
}

void
reinstate_user_modes()
{
	char	modes[10];
	char	*c;

	if (get_server_version(parsing_server_index) < Server2_7)
		return;
	c = modes;
	if (get_server_flag(parsing_server_index, USER_MODE_W))
		*c++ = 'w';
	if (get_server_flag(parsing_server_index, USER_MODE_S))
		*c++ = 's';
	if (get_server_flag(parsing_server_index, USER_MODE_I))
		*c++ = 'i';
	*c = '\0';
	if (c != modes)
		send_to_server("MODE %s +%s",
			get_server_nickname(parsing_server_index),
		modes);
}
