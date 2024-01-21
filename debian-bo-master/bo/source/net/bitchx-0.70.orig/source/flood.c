/*
 * flood.c: handle channel flooding. 
 *
 * This attempts to give you some protection from flooding.  Basically, it keeps
 * track of how far apart (timewise) messages come in from different people.
 * If a single nickname sends more than 3 messages in a row in under a
 * second, this is considered flooding.  It then activates the ON FLOOD with
 * the nickname and type (appropriate for use with IGNORE). 
 *
 * Thanks to Tomi Ollila <f36664r@puukko.hut.fi> for this one. 
 */


#include "irc.h"

#include "alias.h"
#include "hook.h"
#include "ircaux.h"
#include "ignore.h"
#include "flood.h"
#include "vars.h"
#include "output.h"
#include "list.h"
#include "misc.h"
#include "server.h"
#include "userlist.h"
#include "timer.h"
#include "ignore.h"
#include "status.h"

static	char	*ignore_types[NUMBER_OF_FLOODS] =
{
	"MSG",
	"PUBLIC",
	"NOTICE",
	"WALL",
	"WALLOP",
	"CTCP",
	"INVITE",
	"CDCC",
	"ACTION"
};

typedef struct flood_stru
{
	char	nick[NICKNAME_LEN + 1];
	char	channel[BIG_BUFFER_SIZE+1];
	int	type;
	char	flood;
	long	cnt;
	time_t	start;
}	Flooding;

static Flooding *flood = NULL;

extern	char	*FromUserHost;
extern	unsigned int window_display;
extern	int	from_server;
void flood_prot _((char *, char *, char *, int, int, char *));

static double allow_flood = 0.0;
static double this_flood = 0.0;

#define NO_RESET 0
#define RESET 1

int get_flood_rate(int type, ChannelList * channel)
{
	int flood_rate = get_int_var(FLOOD_RATE_VAR);
	if (channel)
	{
		switch(type)
		{
			case JOIN_FLOOD:
				flood_rate = channel->set_joinflood_time;
				break;
			case PUBLIC_FLOOD:
				flood_rate = channel->set_pubflood_time;
				break;
			case NICK_FLOOD:
				flood_rate = channel->set_nickflood_time;
				break;
			case KICK_FLOOD:
				flood_rate = channel->set_kickflood_time;
				break;
			case DEOP_FLOOD:
				flood_rate = channel->set_deopflood_time;
				break;
			default:
				break;
		}
	}
	else
	{
		switch(type)
		{
			case CDCC_FLOOD:
			case CTCP_FLOOD:
				flood_rate = get_int_var(CDCC_FLOOD_RATE_VAR);
			case CTCP_ACTION_FLOOD:
			default:
				break;
		}
	}
	return flood_rate;
}

int get_flood_count(int type, ChannelList * channel)
{
	int flood_count = get_int_var(FLOOD_AFTER_VAR);
	if (channel) {
		switch(type)
		{
			case JOIN_FLOOD:
				flood_count = channel->set_kick_on_joinflood;
				break;
			case PUBLIC_FLOOD:
				flood_count = channel->set_kick_on_pubflood;
				break;
			case NICK_FLOOD:
				flood_count = channel->set_kick_on_nickflood;
				break;
			case KICK_FLOOD:
				flood_count = channel->set_kick_on_kickflood;
				break;
			case DEOP_FLOOD:
				flood_count = channel->set_deop_on_deopflood;
				break;
			default:
			break;
		}
	} 
	else
	{
		switch(type)
		{
			case CDCC_FLOOD:
			case CTCP_FLOOD:
				flood_count = get_int_var(CDCC_FLOOD_AFTER_VAR);
			case CTCP_ACTION_FLOOD:
			default:
				break;
		}
	}
	return flood_count;
}

int set_nick_flood(int type, time_t flood_time, int reset, NickList *tmpnick)
{
	if (!tmpnick)
		return 0;
	switch(type)
	{
		case JOIN_FLOOD:
			if (reset == RESET)
			{
				tmpnick->joincount = 0; 
				tmpnick->jointime = flood_time;
			} else tmpnick->joincount++;
			break;
		case PUBLIC_FLOOD:
			if (reset == RESET)
			{
				tmpnick->floodcount = 0;
				tmpnick->floodtime = tmpnick->idle_time = flood_time;
			} else tmpnick->floodcount++;
			break;
		case NICK_FLOOD:
			if (reset == RESET)
			{
				tmpnick->nickcount = 0;
				tmpnick->nicktime = flood_time;
			} else tmpnick->nickcount++;
			break;
		case KICK_FLOOD:
			if (reset == RESET)
			{
				tmpnick->kickcount = 0;
				tmpnick->kicktime = flood_time;
			} else tmpnick->kickcount++;
			break;
		case DEOP_FLOOD:
			if (reset == RESET)
			{
				tmpnick->dopcount = 0;
				tmpnick->doptime = flood_time;
			} else tmpnick->dopcount++;
			break;
		default:
		break;
	}
	return 1;
}

int is_other_flood(ChannelList *channel, NickList *tmpnick, int type)
{
time_t diff = 0, flood_time = 0;
int doit = 0;
int count = 0;
int flood_rate = 0, flood_count = 0;

	flood_time = time(NULL);
	context;
	
	if (!channel || !tmpnick)
		return 0;
		
	switch(type)
	{
		case JOIN_FLOOD:
			if (!channel->set_joinflood)
				break;
			diff = flood_time - tmpnick->jointime;
			doit = 1; count = tmpnick->joincount;
			break;
		case PUBLIC_FLOOD:
			if (!channel->set_pubflood)
				break;
			diff = flood_time - tmpnick->floodtime;
			doit = 1; count = tmpnick->floodcount;
			break;
		case NICK_FLOOD:
			if (!channel->set_nickflood)
				break;
			diff = flood_time - tmpnick->nicktime;
			doit = 1; count = tmpnick->nickcount;			
			break;
		case DEOP_FLOOD:
			if (!channel->set_deopflood)
				break;
			diff = flood_time - tmpnick->doptime;
			doit = 1; count = tmpnick->dopcount;
			break;
		case KICK_FLOOD:
			if (!channel->set_kickflood)
				break;
			diff = flood_time - tmpnick->kicktime;
			doit = 1; count = tmpnick->kickcount;
			break;
		default:
			doit = 0;
			break;
	}
	if (doit)
	{
		flood_count = get_flood_count(type, channel);
		flood_rate = get_flood_rate(type, channel);
		if ((tmpnick->userlist && tmpnick->userlist->level >= 90) || tmpnick->botlist)
		{
			set_nick_flood(type, flood_time, NO_RESET, tmpnick);
			return 0;
		}
		if (flood_rate && ((diff == 0) || (diff < flood_rate)))
		{
			if (count >= flood_count)
			{
				set_nick_flood(type, 0L, RESET, tmpnick);
				return diff ? diff : 1;
			} else
				set_nick_flood(type, flood_time, NO_RESET, tmpnick);
		}
		else
			set_nick_flood(type, flood_time, RESET, tmpnick);	
	} 
	return 0;
} 

/*
 * check_flooding: This checks for message flooding of the type specified for
 * the given nickname.  This is described above.  This will return 0 if no
 * flooding took place, or flooding is not being monitored from a certain
 * person.  It will return 1 if flooding is being check for someone and an ON
 * FLOOD is activated. 
 */
int check_flooding(char *nick, int type, char *line, char *channel)
{
	static	int	users = 0,
			pos = 0;
	int	i;
	time_t	flood_time = 0, diff = 0;
	Flooding *tmp;
	int flood_rate, flood_count;				
	if (users != get_int_var(FLOOD_USERS_VAR))
	{
		if ((users = get_int_var(FLOOD_USERS_VAR)) == 0)
			return(1);
		if (flood)
			flood = (Flooding *) new_realloc((char *) flood, sizeof(Flooding) * users);
		else
			flood = (Flooding *) new_malloc(sizeof(Flooding) *
				users);
		for (i = 0; i < users; i++)
		{
			*(flood[i]).nick = 0;
			*(flood[i]).channel = 0; 
			flood[i].cnt = 0;
			flood[i].flood = 0;
		}
	}
	if (users == 0)
		return (1);
	for (i = 0; i < users; i++)
	{
		if (*(flood[i].nick))
		{
			if ((my_stricmp(nick, flood[i].nick) == 0) &&
					(type == flood[i].type))
				break;
		}
	}
	flood_time = time(NULL);
	if (i == users)
	{
		tmp = &(flood[pos]);
		pos = (pos + 1) % users;
		strmcpy(tmp->nick, nick, NICKNAME_LEN);
		if (channel)
			strmcpy(tmp->channel, channel, BIG_BUFFER_SIZE);
		tmp->type = type;
		tmp->cnt = 1;
		tmp->start = flood_time;
		tmp->flood = 0;
		return (1);
	}
	else
		tmp = &(flood[i]);

	flood_count = get_flood_count(type, NULL); /* FLOOD_AFTER_VAR */
	flood_rate = get_flood_rate(type, NULL); /* FLOOD_RATE_VAR */
	if (!flood_count || !flood_rate)
		return 1;
	tmp->cnt++;
	diff = flood_time - tmp->start;
	if (tmp->cnt > flood_count)
	{
		if (diff != 0)
			this_flood = (double)tmp->cnt / (double)diff;
		allow_flood = (double)flood_count / (double)flood_rate;
		if ((diff == 0) || (this_flood > allow_flood))
		{
			if (tmp->flood == 0)
			{
				if (get_int_var(FLOOD_WARNING_VAR))
					if (tmp->channel)
						put_it("%s", convert_output_format(get_string_var(FORMAT_FLOOD_VAR), "%s %s %s %s %s", update_clock(GET_TIME), ignore_types[type], nick, FromUserHost, tmp->channel));
					else
						put_it("%s", convert_output_format(get_string_var(FORMAT_FLOOD_VAR), "%s %s %s %s", update_clock(GET_TIME), ignore_types[type], nick, FromUserHost, "unknown"));
				tmp->flood = 1;
				switch(type)
				{
					case MSG_FLOOD:
					case NOTICE_FLOOD:
					case CDCC_FLOOD:
					case CTCP_FLOOD:
						flood_prot(nick, FromUserHost, ignore_types[type], type, get_int_var(IGNORE_TIME_VAR), channel);
						return 0;
					case CTCP_ACTION_FLOOD:
						flood_prot(nick, FromUserHost, ignore_types[CTCP_FLOOD], type, get_int_var(IGNORE_TIME_VAR), channel);
						return 0;
					default:
						break;
				}
			}
			return (do_hook(FLOOD_LIST, "%s %s %s", nick, ignore_types[type],line));
		}
		else
		{
			tmp->flood = 0;
			tmp->cnt = 1;
			tmp->start = flood_time;
		}
	}
	return (1);
}

void flood_prot (char *nick, char *userhost, char *type, int ctcp_type, int ignoretime, char *channel)
{
ChannelList *chan;
NickList *Nick;
char tmp[BIG_BUFFER_SIZE+1];
int	old_window_display;
int	kick_on_flood = 1;

	if ((ctcp_type == CDCC_FLOOD || ctcp_type == CTCP_FLOOD || ctcp_type == CTCP_ACTION_FLOOD) && !get_int_var(CTCP_FLOOD_PROTECTION_VAR))
		return;
	else if (!get_int_var(FLOOD_PROTECTION_VAR))
		return;
	else if (!my_stricmp(nick, get_server_nickname(from_server)))
		return;
	switch (ctcp_type)
	{
		case MSG_FLOOD:
			break;
		case NOTICE_FLOOD:
			break;
		case PUBLIC_FLOOD:
			if (channel)
				kick_on_flood = cget_int_var(PUBFLOOD_CVAR, channel);
		case CTCP_ACTION_FLOOD:
		case CTCP_FLOOD:
		default:
			if (get_int_var(FLOOD_KICK_VAR) && kick_on_flood)
			{
				for (chan = server_list[from_server].chan_list; chan; chan = chan->next)
				{
					if ((Nick = (NickList *)find_in_list((List **)&chan->nicks, nick, 0)))
					{
						if (chan->chop && ((!Nick->userlist && !Nick->botlist) || (Nick->userlist && Nick->userlist->level < 90)))
							send_to_server("KICK %s %s :\002%s\002 flooder", chan->channel, nick, type);
						else
							return;
					}
				}
			}
	}
	if (!ignoretime)
		return;
	sprintf(tmp, "*!*%s %s", userhost, type);
	old_window_display = window_display;
	window_display = 0;
	ignore(NULL, tmp, NULL);
	window_display = old_window_display;
	sprintf(tmp, "%d ^IGNORE *!*%s NONE", ignoretime*60, userhost);
	timercmd("TIMER", tmp, NULL);
	bitchsay("Auto-ignoring %s for %d minutes [\002%s\002 flood]", nick, ignoretime, type);
}
