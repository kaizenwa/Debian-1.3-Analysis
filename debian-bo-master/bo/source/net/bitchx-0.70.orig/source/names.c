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


#include "irc.h"

#include "ircaux.h"
#include "names.h"
#include "flood.h"
#include "window.h"
#include "screen.h"
#include "server.h"
#include "lastlog.h"
#include "list.h"
#include "output.h"
#include "numbers.h"
#include "userlist.h"
#include "timer.h"
#include "input.h"
#include "hook.h"
#include "parse.h"
#include "whowas.h"
#include "misc.h"
#include "bot.h"
#include "vars.h"
#include "tcl_bx.h"
#include "userlist.h"

extern int in_on_who;
extern ChannelList default_statchan;
extern AJoinList *ajoin_list;

static	char	mode_str[] = "iklmnpsta";

static	void	add_to_mode_list _((char *, int, char *));
static	void	check_mode_list_join _((char *, int));
#if 0
static	void	free_channel _((ChannelList **));
#endif
static	void	show_channel _((ChannelList *));
static	void	clear_channel _((ChannelList *));
	char	*recreate_mode _((ChannelList *));
static	void	clear_mode_list _((int));
static	int	decifer_mode _((char *, char *, ChannelList **, u_long *, char *, char *, NickList **, char **));
	char	*channel_key _((char *));
	void	clear_bans _((ChannelList *));

extern  char * function_getkey _((unsigned char *));


static struct modelist
{
	char	*chan;
	int	server;
	char	*mode;
	struct modelist *next;
}	*mode_list = NULL;


static
struct  joinlist
{
	char    *chan;
	int     server,
	gotinfo,
	winref;
	struct  timeval tv;
	struct  joinlist        *next;
}	*join_list = NULL;
                                                        
/* clear_channel: erases all entries in a nick list for the given channel */
static	void
clear_channel(chan)
	ChannelList *chan;
{
	NickList *tmp,
		*next;

	context;
	for (tmp = chan->nicks; tmp; tmp = next)
	{
		next = tmp->next;
#ifdef WANT_STRUCT
		update_structs(0, tmp);
#endif
		new_free(&(tmp->host));
		new_free(&(tmp->nick));
		new_free(&(tmp->server));
		new_free((char **)&tmp);
	}
	chan->nicks = NULL;
}

extern	ChannelList *lookup_channel(char *channel, int server, int unlink)
{
	ChannelList	*chan = NULL,
			*last = NULL;
	if (server == -1)
		server = primary_server;
	if (server == -1 || !server_list[server].chan_list || !channel || !*channel)
		return NULL;
	chan = server_list[server].chan_list;
	context;
	if (!chan)
		return NULL;
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

#ifdef __STDC__
extern void set_waiting_channel (int i)
#else
extern void set_waiting_channel(i)
	int	i;
#endif
{
	Window	*tmp;
	int	flag = 1;

	context;
	while (NULL != (tmp = traverse_all_windows(&flag)))
		if (tmp->server == i && tmp->current_channel)
		{
			if (tmp->bind_channel)
				tmp->waiting_channel = tmp->current_channel;
			tmp->current_channel = NULL;
		}
}

/* if the user is on the given channel, it returns 1. */
#ifdef __STDC__
extern int im_on_channel (char *channel)
#else
extern int im_on_channel (channel)
char *channel;
#endif
{
	context;
	return (channel?(lookup_channel(channel, from_server, 0) ? 1 : 0) : 0);
}

/*
 * add_channel: adds the named channel to the channel list.  If the channel
 * is already in the list, then the channel gets cleaned, and ready for use
 * again.   The added channel becomes the current channel as well.
 */
#ifdef __STDC__
ChannelList * add_channel(char *channel, int server)
#else
ChannelList * add_channel(channel, server)
	char	*channel;
	int	server;
#endif
{
	ChannelList *new = NULL;
	WhowasChanList *whowaschan;
	
	context;
	if ((new = lookup_channel(channel, server, CHAN_NOUNLINK)) != NULL)
	{
		new->connected = 1;
		new->mode = 0;
		new->limit = 0;
		new_free(&new->s_mode);
		new->server = server;
		new->window = curr_scr_win;
		malloc_strcpy(&(new->channel), channel);
		clear_channel(new);
		clear_bans(new);
	}
	else
	{
		context;
		if (!(whowaschan = check_whowas_chan_buffer(channel, 1)))
		{
			context;
			new = (ChannelList *) new_malloc(sizeof(ChannelList));
			memcpy(new, &default_statchan, sizeof(ChannelList));
			new->connected = 1;
			get_time(&new->channel_create);
			malloc_strcpy(&(new->channel), channel);
		}
		else
		{
			context;
			new = whowaschan->channellist;
			new_free(&whowaschan->channel);
			new_free((char **)&whowaschan);
			new_free(&(new->key));
			new->mode = 0;
			new_free(&new->s_mode);
			new->limit = new->i_mode = 0;
			clear_channel(new);
			clear_bans(new);
		}
		new->server = server;
		new->window = curr_scr_win;
		new->flags.got_modes = new->flags.got_who = new->flags.got_bans = 1;
		get_time(&new->join_time);
		add_to_list((List **)&(server_list[server].chan_list), (List *)new);
	}
	new->chop = 0;
	new->voice = 0;
	
	if (!is_current_channel(channel, server, 0))
	{
		int	flag = 1;
		Window	*tmp;

		context;
		while ((tmp = traverse_all_windows(&flag)) != NULL)
		{
                        if (!tmp->waiting_channel && !tmp->bind_channel)
                                 continue;
                        if (tmp->server != from_server)
                                continue;
                        if (tmp->bind_channel && !my_stricmp(tmp->bind_channel, channel))
                        {
                                set_channel_by_refnum(tmp->refnum, channel);
                                new->window = tmp;
                                update_all_windows();
                                return new;
                        }
                        if (tmp->waiting_channel && !my_stricmp(tmp->waiting_channel, channel))
                        {
				set_channel_by_refnum(tmp->refnum, channel);
				new->window = tmp;
				new_free(&tmp->waiting_channel);
				update_all_windows();
				return new;
			}
                        set_channel_by_refnum(tmp->refnum, channel);
                        new->window = tmp;
                        update_all_windows();
                        return new;
		}
		set_channel_by_refnum(0, channel);
		new->window = curr_scr_win;
	}
	update_all_windows();
	return new;
}

/*
 * add_to_channel: adds the given nickname to the given channel.  If the
 * nickname is already on the channel, nothing happens.  If the channel is
 * not on the channel list, nothing happens (although perhaps the channel
 * should be addded to the list?  but this should never happen) 
 */
ChannelList *
add_to_channel(char *channel, char *nick, int server, int oper, int voice, char *userhost, char *server1, char *away)
{
	NickList *new = NULL;
	ChannelList *chan = NULL;
	WhowasList *whowas;

	int	ischop = oper;
	
	context;
	if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
	{
		if (*nick == '+')
		{
			nick++;
			if (!my_stricmp(nick, get_server_nickname(server)))
			{
				check_mode_list_join(channel, server);
				chan->voice = 1;
			}
		}
		if (ischop)
		{
			if (!my_stricmp(nick, get_server_nickname(server)))
			{
				check_mode_list_join(channel, server);
				chan->chop = 1;
			}
		}
		if (!(new = (NickList *) find_in_list((List **)&(chan->nicks), nick, 0)))
		{
			context;
			if (!(whowas = check_whowas_buffer(nick, userhost, channel, 1)))
			{
				new = (NickList *) new_malloc(sizeof(NickList));

				new->idle_time = new->kicktime = 
				new->doptime = new->nicktime = new->floodtime = time(NULL);

				new->nickcount = new->dopcount = 
				new->kickcount = new->floodcount = 0;

				malloc_strcpy(&(new->nick), nick);
			} 
			else
			{
				new = whowas->nicklist;
				new_free(&whowas->channel);
				new_free(&whowas->server1);
				new_free(&whowas->server2);
				new_free((char **)&whowas);
				malloc_strcpy(&(new->nick), nick);
				new->sent_reop = new->sent_deop = 0;
				new->idle_time = new->kicktime = 
				new->doptime = new->nicktime = new->floodtime = time(NULL);

				new->nickcount = new->dopcount = 
				new->kickcount = new->floodcount = 0;
			}
			context;
			add_to_list((List **) &(chan->nicks), (List *) new);
			update_stats(JOINLIST, chan->channel, nick, chan, 0);
			if (!(new->botlist = lookup_userlevelc(nick, userhost, channel, Bot_list)))
				new->userlist = lookup_userlevelc("*", userhost, channel, user_list);
			new->shitlist = nickinshit(nick, userhost);
		}
		new->chanop = ischop;
		new->voice = voice;
		if (away)
		{
			new->away = *away;
			new->ircop = *(away+1) == '*';
		}
		if (server1)
			malloc_strcpy(&new->server, server1);
		if (userhost)
			malloc_strcpy(&new->host, userhost);
	}
#ifdef WANT_STRUCT
	update_structs(2, new);
#endif
	return chan;
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
char	*recreate_mode(ChannelList *chan)
{
	int	mode_pos = 0,
		mode;
	static	char	*s;
	char	buffer[BIG_BUFFER_SIZE + 1];

	chan->i_mode = chan->mode;
	buffer[0] = '\0';
	s = buffer;
	mode = chan->mode;
	context;
	if (!mode)
		return NULL;
	while (mode)
	{
		if (mode % 2)
			*s++ = mode_str[mode_pos];
		mode /= 2;
		mode_pos++;
	}
	if (chan->key && *chan->key)
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
static	int decifer_mode(from, mode_string, channel, mode, chop, voice, nicks, key)
	char	*from;
register char	*mode_string;
	ChannelList **channel;
	u_long	*mode;
	char	*chop;
	char	*voice;
	NickList **nicks;
	char	**key;
{

	char	*limit = 0;
	char	*person;
	int	add = 0;
	int	limit_set = 0;
	int	limit_reset = 0;
	int	splitter = 0;
	char	*rest,
		*the_key;
	
	NickList *ThisNick = NULL;
	BanList *new;
	unsigned int	value = 0;
	int	its_me = 0;
		
	if (!(mode_string = next_arg(mode_string, &rest)))
		return -1;

	context;

	its_me = !my_stricmp(from, get_server_nickname(from_server)) ? 1 : 0;
	splitter = match("*.*.*", from);

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
		case 'a':
			value = MODE_ANON;
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
		{
			if ((person = next_arg(rest, &rest)) && (!my_stricmp(person, get_server_nickname(from_server))))
			{
				if (add && add!=(*channel)->chop && !in_join_list((*channel)->channel, from_server))
				{
					
					register NickList * tmp;
					*chop = (*channel)->chop = add;
					for(tmp=(*channel)->nicks; tmp ;tmp=tmp->next) 
					      check_auto((*channel)->channel,tmp->nick,tmp->host,*channel);
					check_shit(*channel);
					flush_mode_all(*channel);
				}
				 else
				 {
				 	if(!add && add!=(*channel)->chop && !in_join_list((*channel)->channel, from_server))
				 	{
				 		register NickList * tmp;
						for (tmp=(*channel)->nicks; tmp ;tmp=tmp->next) 
							tmp->sent_reop=tmp->sent_deop=0;
					}	 
  				 }				   	
				*chop = add;
				(*channel)->chop = add;
			}
			if ((ThisNick = (NickList *) list_lookup((List **)nicks, person, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
				ThisNick->chanop=add;
			update_stats(add ? MODEOPLIST: MODEDEOPLIST, (*channel)->channel, from, *channel, splitter);
#if 0
			if (ThisNick)
			{
				if (add)
				{
					if (ThisNick->sent_reop)
				 		ThisNick->sent_reop--;
				} 	
				else if (ThisNick->sent_deop)
				        ThisNick->sent_deop--;
			}
#endif
			if (!its_me)
			{
				if (add && splitter)
					check_hack(person, *channel, ThisNick, from);
				else if (!add)
					check_prot(from, person, *channel, NULL, ThisNick);
				else if (ThisNick)
					check_auto((*channel)->channel,ThisNick->nick,ThisNick->host,*channel);
			}
			break;
		}			
		case 'k':
			value = MODE_KEY;
			the_key = next_arg(rest, &rest);
			if (add)
				malloc_strcpy(key, the_key);
			else
				new_free(key);
			break;	
		case 'v':
			if ((person = next_arg(rest, &rest)))
			{
				ThisNick = (NickList *) list_lookup((List **)nicks, person,
        	                	!USE_WILDCARDS, !REMOVE_FROM_LIST);
				if (ThisNick)
					ThisNick->voice=add;
				if (!my_stricmp(person, get_server_nickname(from_server)))
					(*channel)->voice = add;
			}
			break;
		case 'b':
			person = next_arg(rest, &rest);
			if (!person || !*person)
				break;
			update_stats(add?MODEBANLIST:MODEUNBANLIST, (*channel)->channel, from, *channel, splitter);
			if (add)
			{
				ThisNick = (NickList *) list_lookup((List **)nicks, person, !USE_WILDCARDS, !REMOVE_FROM_LIST);
				if (!(new = (BanList *)find_in_list((List **)&(*channel)->bans, person, 0)) || my_stricmp(person, new->ban))
				{
					new = (BanList *) new_malloc(sizeof(BanList));
					malloc_strcpy(&new->ban, person);
					add_to_list((List **)&(*channel)->bans, (List *)new);
				} 
				new->sent_unban = 0;
				if (!new->setby)
					malloc_strcpy(&new->setby, from?from:get_server_name(from_server));
				new->time = time(NULL);
				if (!its_me)
					check_prot(from, person, *channel, new, ThisNick);
			} 
			else
			{
				if ((new = (BanList *)remove_from_list((List **)&(*channel)->bans, person)))
				{
					new_free(&new->setby);
					new_free(&new->ban);
					new_free((char **)&new);
				}
				check_shit(*channel);
			}
			break;
		}
		if (add)
			*mode |= value;
		else
			*mode &= ~value;
	}
	flush_mode_all(*channel);
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

	context;
	if ((tmp = lookup_channel(channel, server, CHAN_NOUNLINK)))
		return recreate_mode(tmp);
	return empty_string;
}

/*
 * update_channel_mode: This will modify the mode for the given channel
 * according the the new mode given.  
 */
void
update_channel_mode(from, channel, server, mode, tmp)
	char	*from;
	char	*channel;
	int	server;
	char	*mode;
	ChannelList *tmp;
{
	int	limit;
	context;
	if (tmp || (channel && (tmp = lookup_channel(channel, server, CHAN_NOUNLINK))))
		if ((limit = decifer_mode(from, mode, &(tmp), &(tmp->mode), &(tmp->chop), &(tmp->voice), &(tmp->nicks), &(tmp->key))) != -1)
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
	context; 
	if ((tmp = lookup_channel(channel, server_index, CHAN_NOUNLINK)))
		return (tmp->mode & mode);
	return 0;
}

#if 0
static	void
free_channel(channel)
	ChannelList **channel;
{
	clear_channel(*channel);
	new_free(&(*channel)->channel);
	new_free(&(*channel)->key);
	new_free((char **)&(*channel));
}
#endif

#ifdef __STDC__
void clear_bans(ChannelList *channel)
#else
void clear_bans(channel)
	ChannelList *channel;
#endif
{
	BanList *bans, 
		*next;
	context;
	if (!channel || !channel->bans)
		return;
	for (bans = channel->bans; bans; bans = next)
	{
		next = bans->next;
		new_free(&bans->setby);
		new_free(&bans->ban);
		new_free((char **)&bans);
	}
	channel->bans = NULL;
}

/*
 * remove_channel: removes the named channel from the
 * server_list[server].chan_list.  If the channel is not on the
 * server_list[server].chan_list, nothing happens.  If the channel was
 * the current channel, this will select the top of the
 * server_list[server].chan_list to be the current_channel, or 0 if the
 * list is empty. 
 */
#ifdef __STDC__
void remove_channel (char *channel, int server)
#else
void
remove_channel(channel, server)
	char	*channel;
	int	server;
#endif
{
	ChannelList *tmp;
	NickList *Nick;
	context;
	if (channel)
	{
		if ((tmp = lookup_channel(channel, server, CHAN_UNLINK)))
		{
			clear_bans(tmp);
			for (Nick = tmp->nicks; Nick; Nick = Nick->next)
				add_to_whowas_buffer(Nick, tmp->channel, Nick->server, NULL);
			tmp->nicks = NULL;
			add_to_whowas_chan_buffer(tmp);
#ifdef WANT_STRUCT
			update_structs(0, tmp);
#endif
		}
		if (is_current_channel(channel, server, -1))
			switch_channels(0,NULL);
		
	/* 1 would reference a window, which we don't want -Soleil */
	}
	else
	{
		ChannelList *next;

		for (tmp = server_list[server].chan_list; tmp; tmp = next)
		{
			next = tmp->next;
			for (Nick = tmp->nicks; Nick; Nick = Nick->next)
				add_to_whowas_buffer(Nick, tmp->channel, Nick->server, get_server_name(server));
			tmp->nicks = NULL;
			clear_bans(tmp);
			add_to_whowas_chan_buffer(tmp);
#ifdef WANT_STRUCT
			update_structs(0, tmp);
#endif
		}
		server_list[server].chan_list = NULL;
	}
	update_all_windows();
}

/*
 * remove_from_channel: removes the given nickname from the given channel. If
 * the nickname is not on the channel or the channel doesn't exist, nothing
 * happens. 
 */
void remove_from_channel(char *channel, char *nick, int server, int netsplit, char *reason)
{
	ChannelList *chan;
	NickList *tmp;
	extern char *last_split_server;
	extern char *last_split_from;
	char buf[BIG_BUFFER_SIZE+1];
	char *server1 = NULL, *server2 = NULL;
	context;
	if (netsplit && reason)
	{
		char *p = NULL;
		strncpy(buf, reason, sizeof(buf)-1);
		if ((p = strchr(buf, ' ')))
		{
			*p++ = '\0';
			server2 = buf;
			server1 = p;
		}		
		if ((!last_split_server && server1) || (server1 && last_split_server && (!match(last_split_server, server1) || strcmp(last_split_server, server1))))
		{
			malloc_strcpy(&last_split_server, server1);
			malloc_strcpy(&last_split_from, server2);
			bitchsay("Press \002Ctrl-F\002 to see who left");
			bitchsay("Press \002Ctrl-E\002 to change to [%s]", server1);
                }
	}
	if (channel)
	{
		if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
			if ((tmp = (NickList *) list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, REMOVE_FROM_LIST)))
			{
				add_to_whowas_buffer(tmp, channel, server1 ? server1 : get_server_name(server), server2);
				if (netsplit)
					add_to_whosplitin_buffer(tmp, channel, server1? server1 : get_server_name(server), server2);
#ifdef WANT_STRUCT
				update_structs(0, tmp);
#endif
			}
	}
	else
	{
		for (chan = server_list[server].chan_list; chan; chan = chan->next)
			if ((tmp = (NickList *) list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, REMOVE_FROM_LIST)))
			{
				add_to_whowas_buffer(tmp, chan->channel, server1 ? server1 : get_server_name(server), server2);

				if (netsplit)
					add_to_whosplitin_buffer(tmp, chan->channel, server1? server1 : get_server_name(server), server2);
#ifdef WANT_STRUCT
				update_structs(0, tmp);
#endif
			}
	}
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
	int t = 0;

	context;
	for (chan = server_list[server].chan_list; chan; chan = chan->next)
	{
		if ((chan->server == server) != 0)
		{
			if ((tmp = (NickList *) list_lookup((List **) &chan->nicks, old_nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			{
				tmp->stat_nicks++;
				if ((t = is_other_flood(chan, tmp, NICK_FLOOD)))
				{
					if (chan->chop)
					{
						if (t)
						{
							if (!tmp->userlist || !tmp->botlist || (tmp->userlist && tmp->userlist->level < 89))
								send_to_server("MODE %s -o+b %s *!*%s", chan->channel, new_nick, tmp->host);
							send_to_server("KICK %s %s :\002Niq flood (%d nicks in %dsecs of %dsecs)\002", chan->channel, new_nick, chan->set_kick_on_nickflood, t, chan->set_nickflood_time);
						}
					}
				}				
				if (chan->chop && chan->set_lamelist)
				{
					if (lame_list && find_in_list((List **)&lame_list, new_nick, 0))
					{
						send_to_server("MODE %s -o+b %s *%s*!*", chan->channel, new_nick, new_nick);
						send_to_server("KICK %s %s :\002Lame Nick detected\002", chan->channel, new_nick);
					}
				}
				new_free(&tmp->nick);
				malloc_strcpy(&tmp->nick, new_nick);
				if (!(tmp->botlist = lookup_userlevelc(tmp->nick, tmp->host, chan->channel, Bot_list)))
					tmp->userlist = lookup_userlevelc("*", tmp->host, chan->channel, user_list);
				tmp->shitlist = nickinshit(tmp->nick, tmp->host);
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
	context;
	chan = lookup_channel(channel, server, CHAN_NOUNLINK);
	if (chan && chan->connected && list_lookup((List **) &(chan->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST))
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
	context;
	if ((chan = lookup_channel(channel, from_server, CHAN_NOUNLINK)) /*&& chan->connected*/)
		if ((Nick = (NickList *) list_lookup((List **)&(chan->nicks),nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
				if (Nick->chanop)
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
	char	*nicks = NULL;
	char	*s;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	*buffer = 0;
	buffer_len = 0;
	context;
	s = recreate_mode(chan);
	for (tmp = chan->nicks; tmp; tmp = tmp->next)
	{
		len = strlen(tmp->nick);
		if (buffer_len + len >= (BIG_BUFFER_SIZE / 2))
		{
			malloc_strcpy(&nicks, buffer);
			say("\t%s %c%s (%s): %s", chan->channel, s ? '+': ' ', s ? s : "<none>", get_server_name(chan->server), nicks);
			*buffer = (char) 0;
			buffer_len = 0;
		}
		strmcat(buffer, tmp->nick, BIG_BUFFER_SIZE);
		strmcat(buffer, " ", BIG_BUFFER_SIZE);
		buffer_len += len + 1;
	}
	malloc_strcpy(&nicks, buffer);
	say("\t%s %c%s (%s): %s", chan->channel, s ? '+':' ', s ? s : "<none>", get_server_name(chan->server), nicks);
	new_free(&nicks);
}

/* list_channels: displays your current channel and your channel list */
void
list_channels()
{
	ChannelList *tmp;
	int	server,
		no = 1;
	context;
	if (server_list[from_server].chan_list)
	{
		if (get_channel_by_refnum(0))
			say("Current channel %s", get_channel_by_refnum(0));
		else
			say("No current channel for this window");
		if ((tmp = server_list[get_window_server(0)].chan_list))
		{
			for (; tmp; tmp = tmp->next)
				show_channel(tmp);
			no = 0;
		}
		if (connected_to_server != 1)
		{
			for (server = 0; server < number_of_servers; server++)
			{
				if (server == get_window_server(0))
					continue;
				say("Other servers:");
				for (tmp = server_list[server].chan_list; tmp; tmp = tmp->next)
					show_channel(tmp);
				no = 0;
			}
		}
	}
	else
		say("You are not on any channels");
}

void
#ifdef __STDC__
switch_channels(char key, char *ptr)
#else
switch_channels(key, ptr)
	char	key;
	char *	ptr;
#endif
{
	ChannelList *	tmp;

	context;
	if (server_list[from_server].chan_list)
	{
		if (get_channel_by_refnum(0))
		{
			if ((tmp = lookup_channel(get_channel_by_refnum(0), from_server, CHAN_NOUNLINK)))
			{
				for (tmp = tmp->next; tmp; tmp = tmp->next)
				{
					if ((tmp->server == from_server) && !is_current_channel(tmp->channel, from_server, 0)
						 /*&& !(is_bound(s, from_server) && curr_scr_win == tmp->window*/)
					{
						set_channel_by_refnum(0, tmp->channel);
						update_all_windows();
						update_all_status(curr_scr_win, NULL, 0);
						set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
						update_input(UPDATE_ALL);
						do_hook(CHANNEL_SWITCH_LIST, "%s", tmp->channel);
#ifdef WANT_TCL
						Tcl_SetVar(interp, "curchan", tmp->channel, TCL_GLOBAL_ONLY);
#endif
						return;
					}
				}
			}
		}
		for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
		{
			if ((tmp->server == from_server) && !is_current_channel(tmp->channel, from_server, 0) && /*!(is_bound(s, from_server) && */curr_scr_win == tmp->window)
			{
				set_channel_by_refnum(0, tmp->channel);
				update_all_windows();
				update_all_status(curr_scr_win, NULL, 0);
				set_input_prompt(curr_scr_win, get_string_var(INPUT_PROMPT_VAR), 0);
				update_input(UPDATE_ALL);
				do_hook(CHANNEL_SWITCH_LIST, "%s", tmp->channel);
#ifdef WANT_TCL
				Tcl_SetVar(interp, "curchan", tmp->channel, TCL_GLOBAL_ONLY);
#endif
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
	context;
	if (server_list[from_server].chan_list)
		for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
			if (tmp->server == from_server && *(tmp->channel) != '#')
				return (tmp->channel);
	return (NULL);
}

void
change_server_channels(old, new)
	int	old,
		new;
{
	ChannelList *tmp;

	context;
	if (new == old)
		return;
	if (new > -1 && server_list[new].chan_list)
		server_list[new].chan_list->server = new;
	if (new > -1 && old > -1)
	{
		tmp = server_list[old].chan_list;
		server_list[new].chan_list = tmp; 
		server_list[old].chan_list = NULL;
	}
}

void
clear_channel_list(server)
	int	server;
{
	ChannelList *tmp,
		*next;
	Window		*ptr;
	int		flag = 1;
	context;
	while ((ptr = traverse_all_windows(&flag)))
		if (ptr->server == server && ptr->current_channel)
			new_free(&ptr->current_channel);
	
	for (tmp = server_list[server].chan_list; tmp; tmp = next)
	{
		NickList *tmp1, *next1;
		for (tmp1 = tmp->nicks; tmp1; tmp1 = next1)
		{
			next1 = tmp1->next;
			add_to_whowas_buffer(tmp1, tmp->channel, tmp1->server, NULL);
#ifdef WANT_STRUCT
			update_structs(0, tmp1);			
#endif
		}
		tmp->nicks = NULL;
		next = tmp->next;
		add_to_whowas_chan_buffer(tmp);
	}

	server_list[server].chan_list = NULL;
	clear_mode_list(server);
	return;
}

/*
 * reconnect_all_channels: used after you get disconnected from a server, 
 * clear each channel nickname list and re-JOINs each channel in the 
 * channel_list ..  
 */
#if 0
Program received signal SIGSEGV, Segmentation fault.
0x808bdeb in reconnect_all_channels (server=0) at names.c:1041
1041                    if (tmp->window && (chan = tmp->window->current_channel) && *chan && !is_bound(chan, tmp->window->server))
(gdb) print chan
#endif

void
reconnect_all_channels(server)
	int	server;
{
	ChannelList *tmp;
	char	*mode;
	context;
	for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
	{
		
		mode = recreate_mode(tmp);
/* CDE possible problems here */
/*
		if (tmp->window && (chan = tmp->window->current_channel) && !is_bound(chan, tmp->window->server))
			malloc_strcpy(&tmp->window->waiting_channel, chan);
*/
		send_to_server("JOIN %s%s%s", tmp->channel, tmp->key ? " " : "", tmp->key ? tmp->key : "");
		if (mode)
			add_to_mode_list(tmp->channel, server, mode);
		add_to_join_list(tmp->channel, server, tmp->window->refnum);
		clear_channel(tmp);
		clear_bans(tmp);
	}
	clear_channel_list(from_server);
	window_check_servers();
	message_from(NULL, LOG_CRAP);
}

char 	* 
channel_key(char *channel) {
        ChannelList *tmp = NULL;

	context;
        for (tmp = server_list[from_server].chan_list; tmp && strcmp(tmp->channel, channel); tmp = tmp->next)
        	;
        return tmp ? ((tmp->key)? tmp->key : empty_string) : empty_string;
}

char	*
what_channel(nick, server)
	char	*nick;
	int	server;
{
	ChannelList *tmp;

	context;
	if (curr_scr_win->current_channel && is_on_channel(curr_scr_win->current_channel, curr_scr_win->server, nick))
		return curr_scr_win->current_channel;

	for (tmp = server_list[from_server].chan_list; tmp; tmp = tmp->next)
		if (list_lookup((List **) &(tmp->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST))
			return tmp->channel;

	return NULL;
}

char	*
walk_channels(nick, init, server)
	int	init;
	char	*nick;
	int	server;
{
	static	ChannelList *tmp = NULL;

	context;
	if (init)
		tmp = server_list[server].chan_list;
	else if (tmp)
		tmp = tmp->next;
	for (;tmp ; tmp = tmp->next)
		if ((tmp->server == from_server) && (list_lookup((List **) &(tmp->nicks), nick, !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			return (tmp->channel);
	return NULL;
}

int get_channel_oper(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *chan;

	context;
	if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
	{
		return chan->chop;
/*
		if ((Nick = (NickList *)list_lookup((List **) &(chan->nicks), get_server_nickname(server), !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			return Nick->chanop;
		return 0;
*/
	}
	return 1;
}

int
get_channel_voice(channel, server)
	char	*channel;
	int	server;
{
	ChannelList *chan;

	context;
	if ((chan = lookup_channel(channel, server, CHAN_NOUNLINK)))
	{
		return chan->voice;
/*
		if ((Nick = (NickList *)list_lookup((List **) &(chan->nicks), get_server_nickname(server), !USE_WILDCARDS, !REMOVE_FROM_LIST)))
			return Nick->voice;
		return 0;
*/
	}
	return 1;
}

extern	void
set_channel_window(window, channel, server)
	Window	*window;
	char	*channel;
	int	server;
{
	ChannelList	*tmp;

	context;
	if (!channel || server < 0)
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
	char	*value = NULL;
	char	buffer[BIG_BUFFER_SIZE + 1];
	
	context;
	*buffer = 0;
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
	context;
	for (i = server + 1; i < number_of_servers; i++)
		for (tmp = server_list[i].chan_list ; tmp; tmp = tmp->next)
			if (tmp->server >= server)
				tmp->server--;
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
				*prev = NULL;

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
			new_free((char **)&tmp);
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
	char			*found = NULL;

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

void show_channel_sync(struct joinlist *tmp, char *chan)
{
struct timeval tv;
	get_time(&tv);
/*	message_to(tmp->winref);*/
	message_from(chan, LOG_CRAP);
	if (do_hook(CHANNEL_SYNCH_LIST, "%s %1.3f", chan, time_diff(tmp->tv,tv)))
		bitchsay("Join to %s was synced in %1.3f secs!!", chan, time_diff(tmp->tv,tv));
	delay_check_auto(chan);
	update_all_status(curr_scr_win, NULL, 0);
	message_from(NULL, LOG_CRAP);
/*	message_to(0);*/
}

/* got_info: increments the gotinfo field when receiving names and mode
   and removes the channel if both have been received */
int got_info(char *chan, int server, int type)
{
	struct	joinlist	*tmp;

	for (tmp = join_list; tmp; tmp = tmp->next)
		if (!my_stricmp(tmp->chan, chan) && tmp->server == server)
		{
			if ((tmp->gotinfo |= type) == (GOTNAMES | GOTMODE | GOTBANS | GOTWHO))
			{
				if (prepare_command(&server, chan, 3))
					show_channel_sync(tmp, chan);
				remove_from_join_list(chan, server);
				return 1;
			}
			return 0;
		}
	return -1;
}

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
	tmp->chan = NULL;
	malloc_strcpy(&tmp->chan, chan);
	tmp->server = server;
	tmp->gotinfo = 0;
	tmp->winref = winref;
	tmp->next = join_list;
	get_time(&tmp->tv);
	join_list = tmp;
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
	context;
	mptr = (struct modelist *) new_malloc(sizeof(struct modelist));
	mptr->chan = NULL;
	malloc_strcpy(&mptr->chan, channel);
	mptr->server = server;
	mptr->mode = NULL;
	malloc_strcpy(&mptr->mode, mode);
	mptr->next = mode_list;
	mode_list = mptr;
}

static	void
check_mode_list_join(channel, server)
	char	*channel;
	int	server;
{
	struct modelist *mptr;

	context;
	if (!channel)
		return;
	for (mptr = mode_list; mptr; mptr = mptr->next)
	{
		if (!my_stricmp(mptr->chan, channel) && mptr->server == server)
		{
			int	old_server = from_server;

			from_server = server;
			send_to_server("MODE %s %s", mptr->chan, mptr->mode);
			from_server = old_server;
			remove_from_mode_list(channel, server);
			return;
		}
	}
}

extern	void
remove_from_mode_list(channel, server)
	char	*channel;
	int	server;
{
	struct modelist	*curr, *next, 	*prev = NULL;

	context;
	for (next = mode_list; next; )
	{
		curr = next;
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
			new_free((char **)&curr);
		}
		else
			prev = curr;
	}
}

extern	void
clear_mode_list(server)
	int	server;
{
	struct modelist	*curr, *next, *prev = NULL;

	context;
	for (next = mode_list; next; )
	{
		curr = next;
		next = curr->next;
		if (curr == mode_list)
			mode_list = curr->next;
		else
			prev->next = curr->next;
		prev = curr;
		new_free(&curr->chan);
		new_free(&curr->mode);
		new_free((char **)&curr);
	}
}

extern	int
chan_is_connected(channel, server)
	char *	channel;
	int	server;
{
	ChannelList *	cp = lookup_channel(channel, server, CHAN_NOUNLINK);

	context;
	if (!cp)
		return 0;

	return (cp->connected);
}
