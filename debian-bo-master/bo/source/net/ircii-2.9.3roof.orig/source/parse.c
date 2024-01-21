/*
 * parse.c: handles messages from the server.   Believe it or not.  I
 * certainly wouldn't if I were you. 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: parse.c,v 1.46.2.2 1996/07/20 19:30:17 mrg Exp $";
#endif

#include "irc.h"

#include "server.h"
#include "names.h"
#include "vars.h"
#include "ctcp.h"
#include "hook.h"
#include "edit.h"
#include "ignore.h"
#include "whois.h"
#include "lastlog.h"
#include "ircaux.h"
#include "funny.h"
#include "crypt.h"
#include "ircterm.h"
#include "flood.h"
#include "window.h"
#include "screen.h"
#include "output.h"
#include "numbers.h"
#include "parse.h"
#include "notify.h"

#define STRING_CHANNEL '+'
#define MULTI_CHANNEL '#'
#define LOCAL_CHANNEL '&'

#define	MAXPARA	15	/* Taken from the ircd */

static	void	BreakArgs _((char *, char **, char **));
static	void	linreply _((char **));
static	void	ping _((char **));
static	void	topic _((char *, char **));
static	void	p_wall _((char *, char **));
static	void	wallops _((char *, char **));
static	void	p_privmsg _((char *, char **));
static	void	msg _((char *, char **));
static	void	p_quit _((char *, char **));
static	void	pong _((char *, char **));
static	void	error _((char *, char **));
static	void	p_channel _((char *, char **));
static	void	p_invite _((char *, char **));
static	void	server_kill _((char *, char **));
static	void	p_nick _((char *, char **));
static	void	mode _((char *, char **));
static	void	kick _((char *, char **));
static	void	part _((char *, char **));

/*
 * joined_nick: the nickname of the last person who joined the current
 * channel 
 */
	char	*joined_nick = (char *) 0;

/* public_nick: nick of the last person to send a message to your channel */
	char	*public_nick = (char *) 0;

/* User and host information from server 2.7 */
	char	*FromUserHost = (char *) 0;

/* doing a PRIVMSG */
	int	doing_privmsg = 0;

/*
 * is_channel: determines if the argument is a channel.  If it's a number,
 * begins with MULTI_CHANNEL and has no '*', or STRING_CHANNEL, then its a
 * channel 
 */
int
is_channel(to)
char	*to;
{
	int	version;

	version = get_server_version(from_server);
	return ((version < Server2_7 && (isdigit(*to) || (*to == STRING_CHANNEL)
		|| *to == '-'))
		|| (version > Server2_5 && *to == MULTI_CHANNEL)
		|| (version > Server2_7 && *to == LOCAL_CHANNEL));
}


char	*
PasteArgs(Args, StartPoint)
	char	**Args;
	int	StartPoint;
{
	int	i;

	for (; StartPoint; Args++, StartPoint--)
		if (!*Args)
			return (char *) 0;
	for (i = 0; Args[i] && Args[i+1]; i++)
		Args[i][strlen(Args[i])] = ' ';
	Args[1] = (char *) 0;
	return Args[0];
}

/*
 * BreakArgs: breaks up the line from the server, in to where its from,
 * setting FromUserHost if it should be, and returns all the arguements
 * that are there.   Re-written by phone, dec 1992.
 */
static	void
BreakArgs(Input, Sender, OutPut)
	char	*Input;
	char	**Sender;
	char	**OutPut;
{
	char	*s = Input,
		*t;
	int	ArgCount = 0;

	/*
	 * Get sender from :sender and user@host if :nick!user@host
	 */
	FromUserHost = (char *) 0;

	if (*Input == ':')
	{
		char	*tmp;
		*Input++ = '\0';
		if ((s = (char *) index(Input, ' ')) != (char *) 0)
			*s++ = '\0';
		*Sender = Input;
		if ((tmp = (char *) index(*Sender, '!')) != (char *) 0)
		{
			*tmp++ = '\0';
			FromUserHost = tmp;
		}
	}
	else
		*Sender = empty_string;

	if (!s)
		return;

	for (;;)
	{
		while (*s == ' ')
			*s++ = '\0';

		if (!*s)
			break;

		if (*s == ':')
		{
			for (t = s; *t; t++)
				*t = *(t + 1);
			OutPut[ArgCount++] = s;
			break;
		}
		OutPut[ArgCount++] = s;
		if (ArgCount >= MAXPARA)
			break;

		for (; *s != ' ' && *s; s++)
			;
	}
	OutPut[ArgCount] = (char *) 0;
}

/* beep_em: Not hard to figure this one out */
void
beep_em(beeps)
	int	beeps;
{
	int	cnt,
		i;

	for (cnt = beeps, i = 0; i < cnt; i++)
		term_beep();
}

/* in response to a TOPIC message from the server */
static	void
topic(from, ArgList)
	char	*from,
		**ArgList;
{
	int	flag;

if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	if (flag == IGNORED)
		return;

	if (!ArgList[1])
	{
		message_from((char *) 0, LOG_CRAP);
		if (do_hook(TOPIC_LIST, "%s * %s", from, ArgList[0]))
			say("%s has changed the topic to %s", from, ArgList[0]);
		message_from((char *) 0, LOG_CURRENT);
	}
	else
	{
		message_from(ArgList[0], LOG_CRAP);
		if (do_hook(TOPIC_LIST, "%s %s %s", from, ArgList[0], ArgList[1]))
			say("%s has changed the topic on channel %s to %s",
				from, ArgList[0], ArgList[1]);
		message_from((char *) 0, LOG_CURRENT);
	}
}

static	void
linreply(ArgList)
	char	**ArgList;
{
	PasteArgs(ArgList, 0);
	say("%s", ArgList[0]);
}

static	void
p_wall(from, ArgList)
	char	*from,
		**ArgList;
{
	int	flag,
		level;
	char	*line;
	char	*high;

if (!from)
		return;
	PasteArgs(ArgList, 0);
	if (!(line = ArgList[0]))
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_WALLS);
	message_from(from, LOG_WALL);
	if (flag != IGNORED)
	{
		if (flag == HIGHLIGHTED)
			high = &highlight_char;
		else
			high = empty_string;
		if ((flag != DONT_IGNORE) && (ignore_usernames & IGNORE_WALLS)
				&& !FromUserHost)
			add_to_whois_queue(from, whois_ignore_walls, "%s",line);
		else
		{
			level = set_lastlog_msg_level(LOG_WALL);
			if (check_flooding(from, WALL_FLOOD, line) &&
					do_hook(WALL_LIST, "%s %s", from, line))
				put_it("%s#%s#%s %s", high, from, high, line);
			if (beep_on_level & LOG_WALL)
				beep_em(1);
			set_lastlog_msg_level(level);
		}
	}
	message_from((char *) 0, LOG_CURRENT);
}

static	void
wallops(from, ArgList)
	char	*from,
		**ArgList;
{
	int	flag;
	char	*line;

if (!from)
		return;
	if (!(line = PasteArgs(ArgList, 0)))
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_WALLOPS);
	if (index(from, '.'))
	{
	/* The old server check, don't use the whois stuff for servers */
		int	level;
		char	*high;

		if (flag != IGNORED)
		{
			if (flag == HIGHLIGHTED)
				high = &highlight_char;
			else
				high = empty_string;
			message_from(from, LOG_WALLOP);
			level = set_lastlog_msg_level(LOG_WALLOP);
			if (do_hook(WALLOP_LIST, "%s S %s", from, line))
				put_it("%s!%s!%s %s", high, from, high, line);
			if (beep_on_level & LOG_WALLOP)
				beep_em(1);
			set_lastlog_msg_level(level);
			message_from((char *) 0, LOG_CURRENT);
		}
	}
	else
	{
		if (get_int_var(USER_WALLOPS_VAR))
		{
			if ((flag != DONT_IGNORE) && (check_flooding(from, WALLOP_FLOOD, line)))
			add_to_whois_queue(from, whois_new_wallops, "%s", line);
		}
		else if (strcmp(from, get_server_nickname(get_window_server(0))) != 0)
			put_it("!%s! %s", from, line);
	}
}

/*ARGSUSED*/
void
whoreply(from, ArgList)
	char	**ArgList,
		*from;
{
	static	char	format[40];
	static	int	last_width = -1;
	int	ok = 1;
	char	*channel,
		*user,
		*host,
		*server,
		*nick,
		*stat,
		*name;
	int	i;

	FILE	*fip;
	char	buf_data[BUFSIZ];

	if (last_width != get_int_var(CHANNEL_NAME_WIDTH_VAR))
	{
		if ((last_width = get_int_var(CHANNEL_NAME_WIDTH_VAR)) != 0)
		    sprintf(format, "%%-%u.%us %%-9s %%-3s %%s@%%s (%%s)",
					(unsigned char) last_width,
					(unsigned char) last_width);
		else
		    strcpy(format, "%s\t%-9s %-3s %s@%s (%s)");
	}
	i = 0;
	channel = user = host = server = nick = stat = name = empty_string;
	if (ArgList[i])
		channel = ArgList[i++];
	if (ArgList[i])
		user = ArgList[i++];
	if (ArgList[i])
		host = ArgList[i++];
	if (ArgList[i])
		server = ArgList[i++];
	if (ArgList[i])
		nick = ArgList[i++];
	if (ArgList[i])
		stat = ArgList[i++];
	PasteArgs(ArgList, i);

	if (*stat == 'S')	/* this only true for the header WHOREPLY */
	{
		channel = "Channel";
		if (((who_mask & WHO_FILE) == 0) || (fopen (who_file, "r")))
		{
			if (do_hook(WHO_LIST, "%s %s %s %s %s %s", channel,
					nick, stat, user, host, ArgList[6]))
				put_it(format, channel, nick, stat, user,
					host, ArgList[6]);
			return;
		}
	}

	if (ArgList[i])
		name = ArgList[i];

	if (who_mask)
	{
		if (who_mask & WHO_HERE)
			ok = ok && (*stat == 'H');
		if (who_mask & WHO_AWAY)
			ok = ok && (*stat == 'G');
		if (who_mask & WHO_OPS)
			ok = ok && (*(stat + 1) == '*');
		if (who_mask & WHO_LUSERS)
			ok = ok && (*(stat + 1) != '*');
		if (who_mask & WHO_CHOPS)
			ok = ok && ((*(stat + 1) == '@') ||
			(*(stat + 2) == '@'));
		if (who_mask & WHO_NAME)
			ok = ok && wild_match(who_name, user);
		if (who_mask & WHO_NICK)
			ok = ok && wild_match(who_nick, nick);
		if (who_mask & WHO_HOST)
			ok = ok && wild_match(who_host, host);
		if (who_mask & WHO_REAL)
			ok = ok && wild_match(who_real, name);
		if (who_mask & WHO_SERVER)
			ok = ok && wild_match(who_server, server);
		if (who_mask & WHO_FILE)
		{
			ok = 0;
			cannot_open = (char *) 0;
			if ((fip = fopen (who_file, "r")) != (FILE *) 0)
			{
				while (fgets (buf_data, BUFSIZ, fip) !=
								(char *) 0)
				{
					buf_data[strlen(buf_data)-1] = '\0';
					ok = ok || wild_match(buf_data, nick);
				}
				fclose (fip);
			} else
				cannot_open = who_file;
		}
	}
	if (ok)
	{
		if (do_hook(WHO_LIST, "%s %s %s %s %s %s", channel, nick,
				stat, user, host, name))
		{
			if (get_int_var(SHOW_WHO_HOPCOUNT_VAR))
				put_it(format, channel, nick, stat, user, host,
					name);
			else
			{
				char	*tmp;

				if ((tmp = (char *) index(name, ' ')) !=
								(char *) 0)
					tmp++;
				else
					tmp = name;
				put_it(format, channel, nick, stat, user, host,
					tmp);
			}
		}
	}
}

static	void
p_privmsg(from, Args)
	char	*from,
		**Args;
{
	int	level,
		flag,
		list_type,
		flood_type,
		log_type;
	unsigned char	ignore_type;
	char	*ptr,
		*to;
	char	*high;
	int	no_flood;

if (!from)
		return;
	PasteArgs(Args, 1);
	to = Args[0];
	ptr = Args[1];
	if (!to || !ptr)
		return;
	if (is_channel(to))
	{
		message_from(to, LOG_MSG);
		malloc_strcpy(&public_nick, from);
		if (!is_on_channel(to, parsing_server_index, from))
		{
			log_type = LOG_PUBLIC;
			ignore_type = IGNORE_PUBLIC;
			list_type = PUBLIC_MSG_LIST;
			flood_type = PUBLIC_FLOOD;
		}
		else
		{
			log_type = LOG_PUBLIC;
			ignore_type = IGNORE_PUBLIC;
			if (is_current_channel(to, parsing_server_index, 0))
				list_type = PUBLIC_LIST;
			else
				list_type = PUBLIC_OTHER_LIST;
			flood_type = PUBLIC_FLOOD;
		}
	}
	else
	{
		message_from(from, LOG_MSG);
		flood_type = MSG_FLOOD;
		if (my_stricmp(to, get_server_nickname(parsing_server_index)))
		{
			log_type = LOG_WALL;
			ignore_type = IGNORE_WALLS;
			list_type = MSG_GROUP_LIST;
		}
		else
		{
			log_type = LOG_MSG;
			ignore_type = IGNORE_MSGS;
			list_type = MSG_LIST;
		}
	}
	flag = double_ignore(from, FromUserHost, ignore_type);
	switch (flag)
	{
	case IGNORED:
		if ((list_type == MSG_LIST) && get_int_var(SEND_IGNORE_MSG_VAR))
			send_to_server("NOTICE %s :%s is ignoring you", from,
					get_server_nickname(parsing_server_index));
		return;
	case HIGHLIGHTED:
		high = &highlight_char;
		break;
	default:
		high = empty_string;
		break;
	}
	ptr = do_ctcp(from, to, ptr);
	if (!ptr || !*ptr)
		return;
	level = set_lastlog_msg_level(log_type);
	if ((flag != DONT_IGNORE) && (ignore_usernames & ignore_type) && !FromUserHost)
		add_to_whois_queue(from, whois_ignore_msgs, "%s", ptr);
	else
	{
		no_flood = check_flooding(from, flood_type, ptr);
		if ((sed == 1) && (!do_hook(ENCRYPTED_PRIVMSG_LIST,"%s %s %s",from, to, ptr)))
			sed = 0;
		else
		{
		switch (list_type)
		{
		case PUBLIC_MSG_LIST:
			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
			    put_it("%s(%s/%s)%s %s", high, from, to, high, ptr);
			break;
		case MSG_GROUP_LIST:
			if (no_flood && do_hook(list_type, "%s %s %s", from, to, ptr))
			    put_it("%s-%s:%s-%s %s", high, from, to, high, ptr);
			break;
		case MSG_LIST:
			if (!no_flood)
				break;
			malloc_strcpy(&recv_nick, from);
			if (away_set)
				beep_em(get_int_var(BEEP_WHEN_AWAY_VAR));
			if (do_hook(list_type, "%s %s", from, ptr))
			{
			    if (away_set)
			    {
				time_t t;
				char *msg = (char *) 0;

				t = time((time_t *) 0);
				msg = (char *) new_malloc(strlen(ptr) + 20);
				sprintf(msg, "%s <%.16s>", ptr, ctime(&t));
				put_it("%s*%s*%s %s", high, from, high, msg);
				new_free(&msg);
			    }
			    else
				put_it("%s*%s*%s %s", high, from, high, ptr);
			}
			break;
		case PUBLIC_LIST:
			doing_privmsg = 1;
			if (no_flood && do_hook(list_type, "%s %s %s", from, 
			    to, ptr))
				put_it("%s<%s>%s %s", high, from, high, ptr);
			doing_privmsg = 0;
			break;
		case PUBLIC_OTHER_LIST:
			doing_privmsg = 1;
			if (no_flood && do_hook(list_type, "%s %s %s", from,
			    to, ptr))
				put_it("%s<%s:%s>%s %s", high, from, to, high,
					ptr);
			doing_privmsg = 0;
			break;
		}
		if (beep_on_level & log_type)
			beep_em(1);
		}
	}
	set_lastlog_msg_level(level);
	message_from((char *) 0, LOG_CURRENT);
}

static	void
msg(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*high,
		*channel,
		*text;
	int	log_type,
		no_flooding;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_PUBLIC);
	switch (flag)
	{
	case IGNORED:
		return;
	case HIGHLIGHTED:
		high = &highlight_char;
		break;
	default:
		high = empty_string;
		break;
	}
	if ((channel = real_channel()) == (char *) 0)
		return;
	text = do_ctcp(from, channel, ArgList[0]);
	if (!text || !*text)
		return;
	malloc_strcpy(&public_nick, from);
	log_type = set_lastlog_msg_level(LOG_PUBLIC);
	no_flooding = check_flooding(from, PUBLIC_FLOOD, text);
	message_from(channel, LOG_PUBLIC);
	if (is_current_channel(channel, parsing_server_index, 0))
	{
		doing_privmsg = 1;
		if (no_flooding && do_hook(PUBLIC_LIST, "%s %s %s", from, channel, text))
			put_it("%s<%s>%s %s", high, from, high, text);
		doing_privmsg = 0;
	}
	else
	{
		doing_privmsg = 1;
		if (no_flooding && do_hook(PUBLIC_OTHER_LIST, "%s %s %s", from,
				channel, text))
			put_it("%s<%s:%s>%s %s", high, from, channel, high,
				text);
		doing_privmsg = 0;
	}
	message_from((char *) 0, LOG_CURRENT);
	if (beep_on_level & LOG_PUBLIC)
		beep_em(1);
	set_lastlog_msg_level(log_type);
}

/*ARGSUSED*/
static	void
p_quit(from, ArgList)
	char	*from,
		**ArgList;
{
	int	one_prints = 0;
	char	*chan;
	char	*Reason;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	if (flag != IGNORED)
	{
		PasteArgs(ArgList, 0);
		Reason = ArgList[0] ? ArgList[0] : "?";
		for (chan = walk_channels(from, 1, parsing_server_index); chan; chan = walk_channels(from, 0, -1))
		{
			message_from(chan, LOG_CRAP);
			if (do_hook(CHANNEL_SIGNOFF_LIST, "%s %s %s", chan, from, Reason))
				one_prints = 1;
		}
		if (one_prints)
		{
			message_from(what_channel(from, parsing_server_index), LOG_CRAP);
			if (do_hook(SIGNOFF_LIST, "%s %s", from, Reason))
				say("Signoff: %s (%s)", from, Reason);
		}
	}
	message_from((char *) 0, LOG_CURRENT);
	remove_from_channel((char *) 0, from, parsing_server_index);
}

/*ARGSUSED*/
static	void
pong(from, ArgList)
	char	*from,
		**ArgList;
{
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	if (flag == IGNORED)
		return;

	if (ArgList[0])
		say("%s: PONG received from %s", ArgList[0], from);
}

/*ARGSUSED*/
static	void
error(from, ArgList)
	char	*from,
		**ArgList;
{
	PasteArgs(ArgList, 0);
	if (!ArgList[0])
		return;
	say("%s", ArgList[0]);
}

static	void
p_channel(from, ArgList)
	char	*from;
	char	**ArgList;
{
	int	join;
	char	*channel;
	int	flag;
	char	*s;
	int	chan_oper = 0, chan_voice = 0;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	if (strcmp(ArgList[0], zero))
	{
		join = 1;
		channel = ArgList[0];
		/*
		 * this \007 should be \a but a lot of compilers are
		 * broken.  *sigh*  -mrg
		 */
		if ((s = index(channel, '\007')))
		{
			*s = '\0';
			while (*++s)
			{
				if (*s == 'o')
					chan_oper = 1;
				if (*s == 'v')
					chan_voice = 1;

			}
		}
		malloc_strcpy(&joined_nick, from);
	}
	else
	{
		join = 0;
		if ((channel = real_channel()) == (char *) 0)
			return;
		message_from(channel, LOG_CRAP);
		if (flag != IGNORED && do_hook(LEAVE_LIST, "%s %s", from, channel))
			say("%s has left channel %s", from, channel);
		message_from((char *) 0, LOG_CURRENT);
	}
	if (!my_stricmp(from, get_server_nickname(parsing_server_index)))
	{
		if (join)
		{
			add_channel(channel, parsing_server_index);
			if (!in_join_list(channel, parsing_server_index))
				add_to_join_list(channel, parsing_server_index, 0);
			if (get_server_version(parsing_server_index) == Server2_5)
				send_to_server("NAMES %s", channel);
			send_to_server("MODE %s", channel);
		}
		else
			remove_channel(channel, parsing_server_index);
	}
	else
	{
		if (join)
			add_to_channel(channel, from, parsing_server_index, chan_oper, chan_voice);
		else
			remove_from_channel(channel, from, parsing_server_index);
	}
	if (join)
	{
		if (!get_channel_oper(channel, parsing_server_index))
			in_on_who = 1;
		message_from(channel, LOG_CRAP);
		if (flag != IGNORED && do_hook(JOIN_LIST, "%s %s", from,
						channel))
		{
			if (FromUserHost)
				say("%s (%s) has joined channel %s", from,
				    FromUserHost, channel);
			else
				say("%s has joined channel %s", from, channel);
		}
		message_from((char *) 0, LOG_CURRENT);
		in_on_who = 0;
	}
}

static	void
p_invite(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*high;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_INVITES);
	switch (flag)
	{
	case IGNORED:
		if (get_int_var(SEND_IGNORE_MSG_VAR))
			send_to_server("NOTICE %s :%s is ignoring you",
				from, get_server_nickname(parsing_server_index));
		return;
	case HIGHLIGHTED:
		high = &highlight_char;
		break;
	default:
		high = empty_string;
		break;
	}
	if (ArgList[0] && ArgList[1])
	{
		if ((flag != DONT_IGNORE) && (ignore_usernames & IGNORE_INVITES)
		    && !FromUserHost)
			add_to_whois_queue(from, whois_ignore_invites,
					"%s", ArgList[1]);
		else
		{
			message_from(from, LOG_CRAP);
			if (do_hook(INVITE_LIST, "%s %s", from, ArgList[1]))
				say("%s%s%s invites you to channel %s", high,
						from, high, ArgList[1]);
			message_from((char *) 0, LOG_CURRENT);
			malloc_strcpy(&invite_channel, ArgList[1]);
			malloc_strcpy(&recv_nick, from);
		}
	}
}

static	void
server_kill(from, ArgList)
	char	*from,
		**ArgList;
{
	/*
	 * this is so bogus checking for a server name having a '.'
	 * in it - phone, april 1993.
	 */
	if (index(from, '.'))
		say("You have been rejected by server %s", from);
	else
	{
		say("You have been killed by operator %s %s", from,
			ArgList[1] ? ArgList[1] : "(No Reason Given)");
#ifndef NO_QUIT_ON_OPERATOR_KILL
		irc_exit();
#endif /* NO_QUIT_ON_OPERATOR_KILL */
	}
	save_chan_from = parsing_server_index;
	close_server(parsing_server_index, empty_string);
	window_check_servers();
	if (!connected_to_server)
		say("Use /SERVER to reconnect to a server");
	else
	{
		save_chan_from = -1;
		clear_channel_list(parsing_server_index);
	}
}

static	void
ping(ArgList)
	char	**ArgList;
{
	PasteArgs(ArgList, 0);
	send_to_server("PONG :%s", ArgList[0]);
}

static	void
p_nick(from, ArgList)
	char	*from,
		**ArgList;
{
	int	one_prints = 0,
		its_me = 0;
	char	*chan;
	char	*line;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	line = ArgList[0];
	if (my_stricmp(from, get_server_nickname(parsing_server_index)) == 0){
		if (parsing_server_index == primary_server)
			strmcpy(nickname, line, NICKNAME_LEN);
		set_server_nickname(parsing_server_index, line);
		its_me = 1;
	}
	if (flag != IGNORED)
	{
		for (chan = walk_channels(from, 1, parsing_server_index); chan;
				chan = walk_channels(from, 0, -1))
		{
			message_from(chan, LOG_CRAP);
			if (do_hook(CHANNEL_NICK_LIST, "%s %s %s", chan, from, line))
				one_prints = 1;
		}
		if (one_prints)
		{
			if (its_me)
				message_from((char *) 0, LOG_CRAP);
			else
				message_from(what_channel(from, parsing_server_index), LOG_CRAP);
			if (do_hook(NICKNAME_LIST, "%s %s", from, line))
				say("%s is now known as %s", from, line);
		}
	}
	rename_nick(from, line, parsing_server_index);
	message_from((char *) 0, LOG_CURRENT);
	if (my_stricmp(from, line))
	{
		notify_mark(from, 0, 0);
		notify_mark(line, 1, 0);
	}
}

static	void
mode(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel;
	char	*line;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	PasteArgs(ArgList, 1);
	channel = ArgList[0];
	line = ArgList[1];
	message_from(channel, LOG_CRAP);
	if (channel && line)
	{
		if (is_channel(channel))
		{
			if (flag != IGNORED && do_hook(MODE_LIST, "%s %s %s",
					from, channel, line))
				say("Mode change \"%s\" on channel %s by %s",
						line, channel, from);
			update_channel_mode(channel, parsing_server_index, line);
		}
		else
		{
			if (flag != IGNORED && do_hook(MODE_LIST, "%s %s %s",
					from, channel, line))
				say("Mode change \"%s\" for user %s by %s",
						line, channel, from);
			update_user_mode(line);
		}
		update_all_status();
	}
	message_from((char *) 0, LOG_CURRENT);
}

static	void
kick(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel,
		*who,
		*comment;

	if (!from)
		return;
	channel = ArgList[0];
	who = ArgList[1];
	comment = ArgList[2];

	if (channel && who)
	{
		if (my_stricmp(who, get_server_nickname(parsing_server_index)) == 0)
		{
			if (comment && *comment)
			{
				message_from(channel, LOG_CRAP);
#ifdef ON_KICK
				if (do_hook(KICK_LIST, "%s %s %s %s", who,
						from, channel, comment))
#endif /* ON_KICK */
					say("You have been kicked off channel %s by %s (%s)",
						channel, from, comment);
				message_from((char *) 0, LOG_CURRENT);
			}
			else
			{
				message_from(channel, LOG_CRAP);
#ifdef ON_KICK
				if (do_hook(KICK_LIST, "%s %s %s", who, from,
						channel))
#endif /* ON_KICK */
					say("You have been kicked off channel %s by %s",
						channel, from);
				message_from((char *) 0, LOG_CURRENT);
			}
			remove_channel(channel, parsing_server_index);
			update_all_status();
		}
		else
		{
			if (comment && *comment)
			{
				message_from(channel, LOG_CRAP);
#ifdef ON_KICK
				if (do_hook(KICK_LIST, "%s %s %s %s", who,
						from, channel, comment))
#endif /* ON_KICK */
					say("%s has been kicked off channel %s by %s (%s)",
						who, channel, from, comment);
				message_from((char *) 0, LOG_CURRENT);
			}
			else
			{
				message_from(channel, LOG_CRAP);
#ifdef ON_KICK
				if (do_hook(KICK_LIST, "%s %s %s", who, from,
						channel))
#endif /* ON_KICK */
					say("%s has been kicked off channel %s by %s",
						who, channel, from);
				message_from((char *) 0, LOG_CURRENT);
			}
			remove_from_channel(channel, who, parsing_server_index);
		}
	}
}

static	void
part(from, ArgList)
	char	*from,
		**ArgList;
{
	char	*channel;
	int	flag;

	if (!from)
		return;
	flag = double_ignore(from, FromUserHost, IGNORE_CRAP);
	channel = ArgList[0];
	in_on_who = 1;
	message_from(channel, LOG_CRAP);
	if (flag != IGNORED && do_hook(LEAVE_LIST, "%s %s", from, channel))
		say("%s has left channel %s", from, channel);
	message_from((char *) 0, LOG_CURRENT);
	if (my_stricmp(from, get_server_nickname(parsing_server_index)) == 0)
	{
		remove_channel(channel, parsing_server_index);
		remove_from_mode_list(channel, parsing_server_index);
	}
	else
		remove_from_channel(channel, from, parsing_server_index);
	in_on_who = 0;
}


/*
 * parse_server: parses messages from the server, doing what should be done
 * with them 
 */
void
parse_server(line)
	char	*line;
{
	server_list[parsing_server_index].parse_server(line);
}

void
irc2_parse_server(line)
	char	*line;
{
	char	*from,
		*comm,
		*end,
		*copy = (char *) 0;
	int	numeric;
	char	**ArgList;
	char	*TrueArgs[MAXPARA + 1];

	if ((char *) 0 == line)
		return;

	end = strlen(line) + line;
	if (*--end == '\n')
		*end-- = '\0';
	if (*end == '\r')
		*end-- = '\0';

	if (*line == ':')
	{
		if (!do_hook(RAW_IRC_LIST, "%s", line + 1))
			return;
	}
	else if (!do_hook(RAW_IRC_LIST, "%s %s", "*", line))
		return;

	malloc_strcpy(&copy, line);
	ArgList = TrueArgs;
	BreakArgs(copy, &from, ArgList);

	if (!(comm = (*ArgList++)))
		return;		/* Empty line from server - ByeBye */

	/*
	 * XXX!!!
	 * this should fail on '1xxx'!!!
	 */
	if (0 != (numeric = atoi(comm)))
		numbered_command(from, numeric, ArgList);
	else if (strcmp(comm, "WHOREPLY") == 0)
		whoreply(from, ArgList);
	else if (strcmp(comm, "NOTICE") == 0)
		parse_notice(from, ArgList);
	else if (strcmp(comm, "PRIVMSG") == 0)
		p_privmsg(from, ArgList);
	else if (strcmp(comm, "NAMREPLY") == 0)
		funny_namreply(from, ArgList);
	else if (strcmp(comm, "JOIN") == 0)
		p_channel(from, ArgList);
	else if (strcmp(comm, "PART") == 0)
		part(from, ArgList);
		/* CHANNEL will go away with 2.6 */
	else if (strcmp(comm, "CHANNEL") == 0)
		p_channel(from, ArgList);
	else if (strcmp(comm, "MSG") == 0)
		msg(from, ArgList);
	else if (strcmp(comm, "QUIT") == 0)
		p_quit(from, ArgList);
	else if (strcmp(comm, "WALL") == 0)
		p_wall(from, ArgList);
	else if (strcmp(comm, "WALLOPS") == 0)
		wallops(from, ArgList);
	else if (strcmp(comm, "LINREPLY") == 0)
		linreply(ArgList);
	else if (strcmp(comm, "PING") == 0)
		ping(ArgList);
	else if (strcmp(comm, "TOPIC") == 0)
		topic(from, ArgList);
	else if (strcmp(comm, "PONG") == 0)
		pong(from, ArgList);
	else if (strcmp(comm, "INVITE") == 0)
		p_invite(from, ArgList);
	else if (strcmp(comm, "NICK") == 0)
		p_nick(from, ArgList);
	else if (strcmp(comm, "KILL") == 0)
		server_kill(from, ArgList);
	else if (strcmp(comm, "MODE") == 0)
		mode(from, ArgList);
	else if (strcmp(comm, "KICK") == 0)
		kick(from, ArgList);
	else if (strcmp(comm, "ERROR") == 0)
		error(from, ArgList);
	else if (strcmp(comm, "ERROR:") == 0) /* Server bug makes this a must */
		error(from, ArgList);
	else
	{
		PasteArgs(ArgList, 0);
		if (from)
			say("Odd server stuff: \"%s %s\" (%s)", comm,
				ArgList[0], from);
		else
			say("Odd server stuff: \"%s %s\"", comm, ArgList[0]);
	}
	new_free(&copy);
	from_server = -1;
}
