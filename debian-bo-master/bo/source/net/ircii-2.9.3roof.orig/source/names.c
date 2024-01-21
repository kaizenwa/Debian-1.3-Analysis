/*
 * names.c: This here is used to maintain a list of all the people currently
 * on your channel.  Seems to work 
 *
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 */

#ifndef lint
static	char	rcsid[] = "@(#)$Id: names.c,v 1.47.2.1 1996/07/20 19:30:22 mrg Exp $";
#endif

#include "irc.h"

#include "ircaux.h"
#include "names.h"
#include "window.h"
#include "screen.h"
#include "server.h"
#include "lastlog.h"
#include "list.h"
#include "output.h"

static	char	mode_str[] = "iklmnpst";

static	void	add_to_mode_list _((char *, int, char *));
static	void	check_mode_list_join _((char *, int));
static	void	free_channel _((ChannelList **));
static	void	show_channel _((ChannelList *));
static	void	clear_channel _((ChannelList *));
static	char	*recreate_mode _((ChannelList *));
static	int	decifer_mode _((char *, u_long *, char *, NickList **, char **));

/*
 * XXXX should make these part of the server struct
 */
static
struct modelist
{
	char	*chan;
	int	server;
	char	*mode;
	struct modelist *next;
}	*mode_list = (struct modelist *) 0;

static
struct	joinlist
{
	char	*chan;
	int	server,
		gotinfo,
		winref;
	struct	joinlist	*next;
}	*join_list = (struct joinlist *) 0;

/* add_to_join_list: registers a channel we're trying to join */
void
add_to_join_list(chan, server, winref)
	char	*chan;
	int	server,
		winref;
{
	struct	joinlist	*tmp;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (!my_stricmp(chan, tmp->chan) && tmp->server == server)
		{
			tmp->winref = winref;
			return;
		}
	tmp = (struct joinlist *) new_malloc(sizeof(struct joinlist));
	tmp->chan = (char *) 0;
	malloc_strcpy(&tmp->chan, chan);
	tmp->server = server;
	tmp->gotinfo = 0;
	tmp->winref = winref;
	tmp->next = join_list;
	join_list = tmp;
}

/* remove_from_join_list: called when mode and names have been received or
   when access has been denied */
void
remove_from_join_list(chan, server)
	char	*chan;
	int	server;
{
	struct	joinlist	*tmp,
				*next,
				*prev = (struct joinlist *) 0;

	for (tmp = join_list; tmp; tmp = tmp->next)
	{
		next = tmp->next;
		if (!my_stricmp(chan, tmp->chan) && tmp->server == server)
		{
			if (tmp == join_list)
				join_list = next;
			else
				prev->next = next;
			new_free(&tmp->chan);
			new_free(&tmp);
			return;
		}
		else
			prev = tmp;
	}
}

/* get_win_from_join_list: returns the window refnum associated to the channel
   we're trying to join */
int
get_win_from_join_list(chan, server)
	char	*chan;
	int	server;
{
	struct	joinlist	*tmp;
	int			found = 0;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (!my_stricmp(tmp->chan, chan) && tmp->server == server)
			found = tmp->winref;

	return found;
}

/* get_chan_from_join_list: returns a pointer on the name of the first channel
   entered into join_list for the specified server */
char	*
get_chan_from_join_list(server)
	int	server;
{
	struct	joinlist	*tmp;
	char			*found = (char *) 0;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (tmp->server == server)
			found = tmp->chan;

	return found;
}

/* in_join_list: returns 1 if we're attempting to join the channel */
int
in_join_list(chan, server)
	char 	*chan;
	int	server;
{
	struct	joinlist	*tmp;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (!my_stricmp(tmp->chan, chan) && tmp->server == server)
			return 1;
	return 0;
}

/* got_info: increments the gotinfo field when receiving names and mode
   and removes the channel if both have been received */
void
got_info(chan, server, type)
	char	*chan;
	int	server,
		type;
{
	struct	joinlist	*tmp;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (!my_stricmp(tmp->chan, chan) && tmp->server == server)
		{
			if ((tmp->gotinfo |= type) == (GOTNAMES | GOTMODE))
				remove_from_join_list(chan, server);
			return;
		}
}

/* clear_channel: erases all entries in a nick list for the given channel */
static	void
clear_channel(chan)
	ChannelList *chan;
{
	NickList *tmp,
		*next;

	for (tmp = chan->nicks; tmp; tmp = next)
	{
		next = tmp->next;
		new_free(&(tmp->nick));
		new_free(&tmp);
	}
	chan->nicks = (NickList *) 0;
}

extern	ChannelList *
lookup_channel(channel, server, unlink)
	char	*channel;
	int	server;
	int	unlink;
{
	ChannelList	*chan = server_list[server].chan_list,
			*last = (ChannelList *) 0;

	if (server == -1)
		server = primary_server;
	while (chan)
	{
		if (chan->server == server && !my_stricmp(chan->channel, channel))
		{
			if (unlink == CHAN_UNLINK)
			{
				if (last)
					last->next = chan->next;
				else
					server_list[server].chan_list = chan->next;
			}
			break;
		}
		last = chan;
		chan = chan->next;
	}
	return chan;
}

/*
 * add_channel: adds the named channel to the channel list.  If the channel
 * is already in the list, then the channel gets cleaned, and ready for use
 * again.   The added channel becomes the current channel as well.
 */
void
add_channel(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *new;
	int	do_add;

	if ((new = lookup_channel(channel, server, CHAN_NOUNLINK)))
	{
#if 0					/* i think this is NOT needed -mrg */
		new_free(&new->key);
#endif
		new_free(&new->s_mode);
		clear_channel(new);
		do_add = 0;
	}
	else
	{
		new = (ChannelList *) new_malloc(sizeof(ChannelList));
		new->channel = (char *) 0;
		new->s_mode = (char *) 0;
		new->key = (char *) 0;
		new->nicks = (NickList *) 0;
		do_add = 1;
	}
	new->mode = 0;
	new->limit = 0;
	new->chop = 0;
	new->server = server;
	new->window = curr_scr_win;
	new->bound = 0;
	new->connected = 1;
	malloc_strcpy(&new->channel, channel);
	if (do_add)
		add_to_list((List **) &server_list[server].chan_list, (List *) new);
	if (!is_current_channel(channel, server, 0))
	{
		int	expected,
			flag = 1;
		Window	*tmp,
			*possible = (Window *) 0;

		expected = get_win_from_join_list(channel, server);

		while ((tmp = traverse_all_windows(&flag)))
		{
			if (tmp->server == server)
			{
				if (tmp->refnum == expected)
				{	
					set_channel_by_refnum(tmp->refnum, channel);
					new->window = tmp;
					update_all_status();
					return;
				}
				else if (!possible)
					possible = tmp;
			}
		}
		if (possible)
		{
			set_channel_by_refnum(possible->refnum, channel);
			new->window = possible;
			update_all_status();
			return;
		}
		set_channel_by_refnum(0, channel);
		new->window = curr_scr_win;
	}
	update_all_windows();
}

/*
 * add_to_channel: adds the given nickname to the given channel.  If the
 * nickname is already on the channel, nothing happens.  If the channel is
 * not on the channel list, nothing happens (although perhaps the channel
 * should be addded to the list?  but this should never happen) 
 */
void
add_to_channel(channel, nick, server, oper, voice)
	char	*channel;
	char	*nick;
	int	server;
	int	oper;
	int	voice;
{
	NickList *new;
	ChannelList *chan;
	int	ischop = oper;

	if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
	{
		if (*nick == '+')
			nick++;
		if (*nick == '@')
		{
			nick++;
			if (my_stricmp(nick, get_server_nickname(server)) == 0)
			{
				check_mode_list_join(channel, server);
				chan->chop = 1;
			}
			ischop = 1;
		}

		if ((new = (NickList *) remove_from_list((List **) &(chan->nicks), nick)))
		{
			new_free(&(new->nick));
			new_free(&new);
		}
		new = (NickList *) new_malloc(sizeof(NickList));
		new->nick = (char *) 0;
		new->chanop = ischop;
		malloc_strcpy(&(new->nick), nick);
		add_to_list((List **) &(chan->nicks), (List *) new);
	}
	notify_mark(nick, 1, 0);
}


/*
 * recreate_mode: converts the bitmap representation of a channels mode into
 * a string 
 *
 * This malloces what it returns, but nothing that calls this function
 * is expecting to have to free anything.  Therefore, this function
 * should not malloc what it returns.  (hop)
 *
 * but this leads to horrible race conditions, so we add a bit to
 * the channel structure, and cache the string value of mode, and
 * the u_long value of the cached string, so that each channel only
 * has one copy of the string.  -mrg, june '94.
 */
static	char	*
recreate_mode(chan)
	ChannelList *chan;
{
	int	mode_pos = 0,
		mode;
	static	char	*s;

	/* first check if cached string value is ok */

	if (chan->mode == chan->i_mode && chan->limit == chan->i_limit)
		return (chan->s_mode);

	chan->i_mode = chan->mode;
	chan->i_limit = chan->limit;
	buffer[0] = '\0';
	s = buffer;
	mode = chan->mode;
	while (mode)
	{
		if (mode % 2)
			*s++ = mode_str[mode_pos];
		mode /= 2;
		mode_pos++;
	}
	if (chan->key)
	{
		*s++ = ' ';
		strcpy(s, chan->key);
		s += strlen(chan->key);
	}
	if (chan->limit)
		sprintf(s, " %d", chan->limit);
	else
		*s = '\0';

	malloc_strcpy(&chan->s_mode, buffer);
	return chan->s_mode;
}

/*
 * decifer_mode: This will figure out the mode string as returned by mode
 * commands and convert that mode string into a one byte bit map of modes 
 */
static	int
decifer_mode(mode_string, mode, chop, nicks, key)
	char	*mode_string;
	u_long	*mode;
	char	*chop;
	NickList **nicks;
	char	**key;
{
	char	*limit = 0;
	char	*person;
	int	add = 0;
	int	limit_set = 0;
	int	limit_reset = 0;
	char	*rest,
		*the_key;
	NickList *ThisNick;
	unsigned char	value = 0;

	if (!(mode_string = next_arg(mode_string, &rest)))
		return -1;
	for (; *mode_string; mode_string++)
	{
		switch (*mode_string)
		{
		case '+':
			add = 1;
			value = 0;
			break;
		case '-':
			add = 0;
			value = 0;
			break;
		case 'p':
			value = MODE_PRIVATE;
			break;
		case 'l':
			value = MODE_LIMIT;
			if (add)
			{
				limit_set = 1;
				if (!(limit = next_arg(rest, &rest)))
					limit = empty_string;
				else if (0 == strncmp(limit, zero, 1))
					limit_reset = 1, limit_set = 0, add = 0;
			}
			else
				limit_reset = 1;
			break;
		case 't':
			value = MODE_TOPIC;
			break;
		case 'i':
			value = MODE_INVITE;
			break;
		case 'n':
			value = MODE_MSGS;
			break;
		case 's':
			value = MODE_SECRET;
			break;
		case 'm':
			value = MODE_MODERATED;
			break;
		case 'o':
			if ((person = next_arg(rest, &rest)) && !my_stricmp(person, get_server_nickname(from_server)))
				*chop = add;
			ThisNick = (NickList *) list_lookup((List **) nicks, person, !USE_WILDCARDS, !REMOVE_FROM_LIST);
			if (ThisNick)
				ThisNick->chanop = add;
			break;
		case 'k':
			value = MODE_KEY;
			the_key = next_arg(rest, &rest);
			if (add)
				malloc_strcpy(key, the_key);
			else
				new_free(key);
			break;	
		case 'v':
		case 'b':
			(void) next_arg(rest, &rest);
			break;
		}
		if (add)
			*mode |= value;
		else
			*mode &= ~value;
	}
	if (limit_set)
		return (atoi(limit));
	else if (limit_reset)
		return(0);
	else
		return(-1);
}

/*
 * get_channel_mode: returns the current mode string for the given channel
 */
char	*
get_channel_mode(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *tmp;

	if ((tmp = lookup_channel(channel, server, CHAN_NOUNLINK)))
		return recreate_mode(tmp);
	return empty_string;
}

/*
 * update_channel_mode: This will modify the mode for the given channel
 * according the the new mode given.  
 */
void
update_channel_mode(channel, server, mode)
	char	*channel;
	int	server;
	char	*mode;
{
	ChannelList *tmp;
	int	limit;

	if ((tmp = lookup_channel(channel, server, CHAN_NOUNLINK)) &&
			(limit = decifer_mode(mode, &(tmp->mode), &(tmp->chop), &(tmp->nicks), &(tmp->key))) != -1)
		tmp->limit = limit;
}

/*
 * is_channel_mode: returns the logical AND of the given mode with the
 * channels mode.  Useful for testing a channels mode 
 */
int
is_channel_mode(channel, mode, server_index)
	char	*channel;
	int	mode;
	int	server_index;
{
	ChannelList *tmp;

	if ((tmp = lookup_channel(channel, server_index, CHAN_NOUNLINK)))
		return (tmp->mode & mode);
	return 0;
}

static	void
free_channel(channel)
	ChannelList **channel;
{
	clear_channel(*channel);
	new_free(&(*channel)->channel);
	new_free(&(*channel)->key);
	new_free(&(*channel));
}

/*
 * remove_channel: removes the named channel from the
 * server_list[server].chan_list.  If the channel is not on the
 * server_list[server].chan_list, nothing happens.  If the channel was
 * the current channel, this will select the top of the
 * server_list[server].chan_list to be the current_channel, or 0 if the
 * list is empty. 
 */
void
remove_channel(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *tmp;

	if (channel)
	{
		if ((tmp = lookup_channel(channel, server, CHAN_UNLINK)))
			free_channel(&tmp);

		(void)is_current_channel(channel, server, -1);
		/* 1 would reference a window, which we don't want -Soleil */
	}
	else
	{
		ChannelList *next;

		for (tmp = server_list[server].chan_list; tmp; tmp = next)
		{
			next = tmp->next;
			free_channel(&tmp);
		}
		server_list[server].chan_list = (ChannelList *) 0;
	}
	update_all_windows();
}

/*
 * remove_from_channel: removes the given nickname from the given channel. If
 * the nickname is not on the channel or the channel doesn't exist, nothing
 * happens. 
 */
void
remove_from_channel(channel, nick, server)
	char	*channel;
	char	*nick;
	int	server;
{
	ChannelList *chan;
	NickList *tmp;

	if (channel)
	{
		if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
		{
			if ((tmp = (NickList *) list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, REMOVE_FROM_LIST)))
			{
				new_free(&(tmp->nick));
				new_free(&tmp);
			}
		}
	}
	else
	{
		for (chan = server_list[server].chan_list; chan; chan = chan->next)
		{
			if ((tmp = (NickList *) list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, REMOVE_FROM_LIST)))
			{
				new_free(&(tmp->nick));
				new_free(&tmp);
			}
		}
	}
	notify_mark(nick, 0, 0);
}

/*
 * rename_nick: in response to a changed nickname, this looks up the given
 * nickname on all you channels and changes it the new_nick 
 */
void
rename_nick(old_nick, new_nick, server)
	char	*old_nick,
		*new_nick;
	int	server;
{
	ChannelList *chan;
	NickList *tmp;

	for (chan = server_list[server].chan_list; chan; chan = chan->next)
	{
		if ((chan->server == server) != 0)
		{
			if ((tmp = (NickList *) list_lookup((List **) &chan->nicks, old_nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			{
				new_free(&tmp->nick);
				malloc_strcpy(&tmp->nick, new_nick);
			}
		}
	}
}

/*
 * is_on_channel: returns true if the given nickname is in the given channel,
 * false otherwise.  Also returns false if the given channel is not on the
 * channel list. 
 */
int
is_on_channel(channel, server, nick)
	char	*channel;
	int	server;
	char	*nick;
{
	ChannelList *chan;

	chan = lookup_channel(channel, server, CHAN_NOUNLINK);
	if (chan && chan->connected	/* channel may be "surviving" from
					   a server disconnect/reconnect,
					   make sure it's connected -Sol */
	&& list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST))
		return 1;
	return 0;
}

int
is_chanop(channel, nick)
	char	*channel;
	char	*nick;
{
	ChannelList *chan;
	NickList *Nick;

	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)) &&
			chan->connected &&	/* channel may be "surviving"
						   from a disconnect/connect
						   check here too -Sol */
			(Nick = (NickList *) list_lookup((List **) &(chan->nicks),
			nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)) &&
			Nick->chanop)
		return 1;
	return 0;
}

static	void
show_channel(chan)
	ChannelList *chan;
{
	NickList *tmp;
	int	buffer_len,
		len;
	char	*nicks = (char *) 0;
	char	*s;

	s = recreate_mode(chan);
	*buffer = (char) 0;
	buffer_len = 0;
	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		len = strlen(tmp->nick);
		if (buffer_len + len >= (BIG_BUFFER_SIZE / 2))
		{
			malloc_strcpy(&nicks, buffer);
			say("\t%s +%s (%s): %s", chan->channel, s, get_server_name(chan->server), nicks);
			*buffer = (char) 0;
			buffer_len = 0;
		}
		strmcat(buffer, tmp->nick, BIG_BUFFER_SIZE);
		strmcat(buffer, " ", BIG_BUFFER_SIZE);
		buffer_len += len + 1;
	}
	malloc_strcpy(&nicks, buffer);
	say("\t%s +%s (%s): %s", chan->channel, s, get_server_name(chan->server), nicks);
	new_free(&nicks);
}

/* list_channels: displays your current channel and your channel list */
void
list_channels()
{
	ChannelList *tmp;
	int	server,
		no = 1;
	int	first;

	if (connected_to_server < 1)
	{
		say("You are not connected to a server, use /SERVER to connect.");
		return;
	}
	if (get_channel_by_refnum(0))
		say("Current channel %s", get_channel_by_refnum(0));
	else
		say("No current channel for this window");
	first = 1;
	for (tmp = server_list[get_window_server(0)].chan_list; tmp; tmp = tmp->next)
	{
		if (first)
			say("You are on the following channels:");
		show_channel(tmp);
		first = 0;
		no = 0;
	}

	if (connected_to_server > 1)
	{
		for (server = 0; server < number_of_servers; server++)
		{
			if (server == get_window_server(0))
				continue;
			first = 1;
			for (tmp = server_list[server].chan_list; tmp; tmp = tmp->next)
			{
				if (first)
					say("Other servers:");
				show_channel(tmp);
				first = 0;
				no = 0;
			}
		}
	}
	if (no)
		say("You are not on any channels");
}

void
#ifdef __STDC__
switch_channels(unsigned char key, char *ptr)
#else
switch_channels(key, ptr)
	unsigned char	key;
	char *	ptr;
#endif
{
	ChannelList *	tmp;
	char *	s;

	if (server_list[from_server].chan_list)
	{
		if (get_channel_by_refnum(0))
		{
			if ((tmp = lookup_channel(get_channel_by_refnum(0), from_server, CHAN_NOUNLINK)))
			{
				for (tmp = tmp->next; tmp; tmp = tmp->next)
				{
					s = tmp->channel;
					if (!is_current_channel(s, from_server, 0) && !(is_bound(s, from_server) && curr_scr_win != tmp->window))
					{
						set_channel_by_refnum(0, s);
						update_all_windows();
						return;
					}
				}
			}
		}
		for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
		{
			s = tmp->channel;
			if (!is_current_channel(s, from_server, 0) && !(is_bound(s, from_server) && curr_scr_win != tmp->window))
			{
				set_channel_by_refnum(0, s);
				update_all_windows();
				return;
			}
		}
	}
}

/* real_channel: returns your "real" channel (your non-multiple channel) */
char	*
real_channel()
{
	ChannelList *tmp;

	if (server_list[from_server].chan_list)
		for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
			if (tmp->server == from_server && *(tmp->channel) != '#')
				return (tmp->channel);
	return ((char *) 0);
}

void
change_server_channels(old, new)
	int	old,
		new;
{
	ChannelList *tmp;

	if (new == old)
		return;
	if (old > -1)
	{
		for (tmp = server_list[old].chan_list; tmp ;tmp = tmp->next)
			tmp->server = new;
		server_list[new].chan_list = server_list[old].chan_list;
	}
	else
		server_list[new].chan_list = (ChannelList *) 0;
}

void
clear_channel_list(server)
	int	server;
{
	ChannelList *tmp,
		*next;
	Window		*ptr;
	int		flag = 1,
			i;

	while ((ptr = traverse_all_windows(&flag)))
		if (ptr->server == server && ptr->current_channel)
			new_free(&ptr->current_channel);
	
	if (save_chan_from == server)
		goto out;
	for (i = 0; i < number_of_servers; i++)
		if (server_list[i].copy_from == server)
			goto out;
	for (tmp = server_list[server].chan_list; tmp; tmp = next)
	{
		next = tmp->next;
		free_channel(&tmp);
	}
	server_list[server].chan_list = (ChannelList *) 0;
	return;
out:
	for (tmp = server_list[server].chan_list; tmp; tmp = tmp->next)
		tmp->connected = 0;
}

/*
 * reconnect_all_channels: used after you get disconnected from a server, 
 * clear each channel nickname list and re-JOINs each channel in the 
 * channel_list ..  
 */
void
reconnect_all_channels(server)
	int	server;
{
	ChannelList *tmp;
	char	*mode, *chan;
	int	copy_server;

	if ((copy_server = server_list[server].copy_from) == -1)
		return;
	for (tmp = server_list[copy_server].chan_list; tmp; tmp = tmp->next)
	{
		mode = recreate_mode(tmp);
		chan = tmp->channel;
		if (get_server_version(server) >= Server2_8)
		{
			send_to_server("JOIN %s%s%s", tmp->channel, tmp->key ? " " : "", tmp->key ? tmp->key : "");
			if ((char *) 0 != mode)
				add_to_mode_list(tmp->channel, server, mode);
		}
		else
			send_to_server("JOIN %s%s%s", tmp->channel, mode ? " " : "", mode ? mode : "");
		add_to_join_list(chan, server, tmp->window->refnum);
	}
	server_list[server].copy_from = -1;
	save_chan_from = -1;	/* seems more adequate to reset it before
				   clearing rather than after -Sol */
	clear_channel_list(copy_server);
	window_check_servers();
}

char	*
what_channel(nick, server)
	char	*nick;
	int	server;
{
	ChannelList *tmp;

	if (curr_scr_win->current_channel && is_on_channel(curr_scr_win->current_channel, curr_scr_win->server, nick))
		return curr_scr_win->current_channel;

	for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
		if (list_lookup((List **) &(tmp->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST))
			return tmp->channel;

	return (char *) 0;
}

char	*
walk_channels(nick, init, server)
	int	init;
	char	*nick;
	int	server;
{
	static	ChannelList *tmp = (ChannelList *) 0;

	if (init)
		tmp = server_list[server].chan_list;
	else if (tmp)
		tmp = tmp->next;
	for (;tmp ; tmp = tmp->next)
		if ((tmp->server == from_server) && (list_lookup((List **) &(tmp->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			return (tmp->channel);
	return (char *) 0;
}

int
get_channel_oper(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *chan;

	if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
		return chan->chop;
	else
		return 1;
}

extern	void
set_channel_window(window, channel, server)
	Window	*window;
	char	*channel;
	int	server;
{
	ChannelList	*tmp;

	if (!channel)
		return;
	for (tmp = server_list[server].chan_list; tmp; tmp = tmp->next)
		if (!my_stricmp(channel, tmp->channel))
		{
			tmp->window = window;
			return;
		}
}

extern	char	*
create_channel_list(window)
	Window	*window;
{
	ChannelList	*tmp;
	char	*value = (char *) 0;

	*buffer = '\0';
	for (tmp = server_list[window->server].chan_list; tmp; tmp = tmp->next)
	{
		strcat(buffer, tmp->channel);
		strcat(buffer, " ");
	}
	malloc_strcpy(&value, buffer);

	return value;
}

extern void
channel_server_delete(server)
	int     server;
{
	ChannelList     *tmp;
	int	i;

	for (i = server + 1; i < number_of_servers; i++)
		for (tmp = server_list[i].chan_list ; tmp; tmp = tmp->next)
			if (tmp->server >= server)
				tmp->server--;
}

static	void
add_to_mode_list(channel, server, mode)
	char	*channel;
	int	server;
	char	*mode;
{
	struct modelist	*mptr;

	if (!channel || !*channel || !mode || !*mode)
		return;
	mptr = (struct modelist *) new_malloc(sizeof(struct modelist));
	mptr->chan = (char *) 0;
	malloc_strcpy(&mptr->chan, channel);
	mptr->server = server;
	mptr->mode = (char *) 0;
	malloc_strcpy(&mptr->mode, mode);
	mptr->next = mode_list;
	mode_list = mptr;
}

static	void
check_mode_list_join(channel, server)
	char	*channel;
	int	server;
{
	struct modelist *mptr = mode_list;

	for (;(struct modelist *) 0 != mptr; mptr = mptr->next)
	{
		if (0 == my_stricmp(mptr->chan, channel)
			&& mptr->server == server)
		{
			int	old_server = from_server;

			from_server = server;
			send_to_server("MODE %s %s", mptr->chan, mptr->mode);
			from_server = old_server;
			return;
		}
	}
	remove_from_mode_list(channel, server);
}

extern	void
remove_from_mode_list(channel, server)
	char	*channel;
	int	server;
{
	struct modelist	*curr,
			*next,
			*prev = (struct modelist *) 0;

	for (curr = mode_list; curr; curr = next)
	{
		next = curr->next;
		if (!my_stricmp(curr->chan, channel) && curr->server == server)
		{
			if (curr == mode_list)
				mode_list = curr->next;
			else
				prev->next = curr->next;
			prev = curr;
			new_free(&curr->chan);
			new_free(&curr->mode);
			new_free(&curr);
		}
		else
			prev = curr;
	}
}

extern	int
chan_is_connected(channel, server)
	char *	channel;
	int	server;
{
	ChannelList *	cp = lookup_channel(channel, server, CHAN_NOUNLINK);

	if (!cp)
		return 0;

	return (cp->connected);
}
