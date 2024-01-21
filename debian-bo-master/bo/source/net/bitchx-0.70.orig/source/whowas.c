/*
 * whowas.c   a linked list buffer of people who have left your channel 
 * mainly used for ban prot and stats stuff.
 * Should even speed stuff up a bit too.
 *
 * Written by Scott H Kilau
 *
 * Copyright(c) 1995
 * Modified Colten Edwards 1996
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT
 */

#include "irc.h"

#include "struct.h"

#include "vars.h"
#include "ircaux.h"
#include "window.h"
#include "whois.h"
#include "hook.h"
#include "input.h"
#include "names.h"
#include "alias.h"
#include "output.h"
#include "numbers.h"
#include "status.h"
#include "screen.h"
#include "edit.h"
#include "config.h"
#include "list.h"
#include "userlist.h"
#include "misc.h"

/*
#include "ctimers.h"
#include "edit2.h"
#include "edit3.h"
#include "edit4.h"
*/
#include "userlist.h"
#include "whowas.h"

WhowasList *whowas_userlist_list = NULL;
WhowasList *whowas_reg_list = NULL;

WhowasList *whowas_splitin_list = NULL;

WhowasChanList *whowas_chan_list = NULL;

static int whowas_userlist_count = 0;
static int whowas_reg_count = 0;
static int whowas_chan_count = 0;

extern	WhowasList *
check_whowas_buffer(nick, userhost, channel, unlink)
char	*nick,
	*userhost,
	*channel;
int	unlink;
{
	WhowasList *tmp, *last = NULL;
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
		if (!my_stricmp(tmp->nicklist->host, userhost) &&
		   !my_stricmp(tmp->channel, channel)) {
			if (unlink) {
				if (last)
					last->next = tmp->next;
				else
					whowas_userlist_list = tmp->next;
				whowas_userlist_count--;
			}
			return(tmp);
		}
		last = tmp;
	}
        last = tmp = NULL;
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
		   if (!my_stricmp(tmp->nicklist->host, userhost) &&
		      !my_stricmp(tmp->channel, channel)) {
			if (unlink) {
				if (last)
					last->next = tmp->next;
				else
					whowas_reg_list = tmp->next;
				whowas_reg_count--;
			}
			return(tmp);
		}
		last = tmp;
	}
	return( NULL );
}


extern	WhowasList *
check_whowas_nick_buffer(nick, channel, unlink)
char	*nick,
	*channel;
int	unlink;
{
	WhowasList *tmp, *last = NULL;
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
		if (!my_stricmp(tmp->nicklist->nick, nick) &&
		   !my_stricmp(tmp->channel, channel)) {
			if (unlink) {
				if (last)
					last->next = tmp->next;
				else
					whowas_userlist_list = tmp->next;
				whowas_userlist_count--;
			}
			return(tmp);
		}
		last = tmp;
	}
        last = tmp = NULL;
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
		   if (!my_stricmp(tmp->nicklist->nick, nick) &&
		      !my_stricmp(tmp->channel, channel)) {
			if (unlink) {
				if (last)
					last->next = tmp->next;
				else
					whowas_reg_list = tmp->next;
				whowas_reg_count--;
			}
			return(tmp);
		}
		last = tmp;
	}
	return( NULL );
}

extern	WhowasList *
check_whosplitin_buffer(nick, userhost, channel, unlink)
char	*nick,
	*userhost,
	*channel;
int	unlink;
{
	WhowasList *tmp, *last = NULL;
	for (tmp = whowas_splitin_list; tmp; tmp = tmp->next) 
	{
		if (!my_stricmp(tmp->nicklist->host, userhost) && channel &&
		   tmp->channel &&
		   !my_stricmp(tmp->channel, channel)) 
		{
			if (unlink) 
			{
				if (last)
					last->next = tmp->next;
				else
					whowas_splitin_list = tmp->next;
			}
			return(tmp);
		}
		last = tmp;
	}
	return( NULL );
}

void
add_to_whowas_buffer(nicklist, channel, server1, server2)
NickList *nicklist;
char *channel;
char *server1;
char *server2;
{
	WhowasList *new;
	WhowasList **slot;

	if (!nicklist)
		return;

	if (nicklist->userlist) {
		if (whowas_userlist_count >= whowas_userlist_max) {
			whowas_userlist_count -=
			   remove_oldest_whowas(&whowas_userlist_list, 0,
			   (whowas_userlist_max + 1) - whowas_userlist_count); 
		}
		new = (WhowasList *) new_malloc(sizeof(WhowasList));
		new->has_ops = nicklist->chanop;
		new->channel = NULL;
		new->server1 = NULL;
		new->server2 = NULL;
		new->nicklist = nicklist;
		new->nicklist->next = NULL;
		malloc_strcpy(&(new->channel), channel);
		if (server1)
			malloc_strcpy(&(new->server1), server1);
		if (server2)
			malloc_strcpy(&(new->server2), server2);
		new->time = time(NULL);

		/* we've created it, now put it in order */
		for (slot = &whowas_userlist_list; *slot; slot = &(*slot)->next)
		{
			if ((*slot)->time > new->time)
				break;
		}
		new->next = *slot;
		*slot = new;
		whowas_userlist_count++;
	}
	else {
		if (whowas_reg_count >= whowas_reg_max) {
			whowas_reg_count -=
			   remove_oldest_whowas(&whowas_reg_list, 0,
			   (whowas_reg_max + 1) - whowas_reg_count); 
		}
		new = (WhowasList *) new_malloc(sizeof(WhowasList));
		new->has_ops = nicklist->chanop;
		new->channel = NULL;
		new->server1 = NULL;
		new->server2 = NULL;
		new->nicklist = (NickList *) nicklist;
		new->nicklist->next = NULL;
		malloc_strcpy(&(new->channel), channel);
		if (server1)
			malloc_strcpy(&(new->server1), server1);
		if (server2)
			malloc_strcpy(&(new->server2), server2);
		new->time = time(NULL);

		/* we've created it, now put it in order */
		for (slot = &whowas_reg_list; *slot; slot = &(*slot)->next)
		{
			if ((*slot)->time > new->time)
				break;
		}
		new->next = *slot;
		*slot = new;
		whowas_reg_count++;
	}
}

void
add_to_whosplitin_buffer(nicklist, channel, server1, server2)
NickList *nicklist;
char *channel;
char *server1;
char *server2;
{
	WhowasList *new;
	WhowasList **slot;

	if (!nicklist)
		return;

	new = (WhowasList *) new_malloc(sizeof(WhowasList));
	new->has_ops = nicklist->chanop;

	new->nicklist = (NickList *)new_malloc(sizeof(NickList)); /*nicklist;*/
	new->nicklist->nick = m_strdup(nicklist->nick);
	new->nicklist->host = m_strdup(nicklist->host);

	malloc_strcpy(&(new->channel), channel);
	if (server1)
		malloc_strcpy(&(new->server1), server1);
	if (server2)
		malloc_strcpy(&(new->server2), server2);
	new->time = time(NULL);

	/* we've created it, now put it in order */
	for (slot = &whowas_splitin_list; *slot; slot = &(*slot)->next)
	{
		if ((*slot)->time > new->time)
			break;
	}
	new->next = *slot;
	*slot = new;
}

int 
remove_oldest_splitin_whowas(list, timet)
WhowasList **list; 
time_t timet;
{
	WhowasList *tmp = NULL;
	int total = 0;
	time_t t;
	t = time(NULL);
	while (*list && ((*list)->time + timet) <= t) {
		tmp = *list;
		new_free(&(tmp->nicklist->nick));
		new_free(&(tmp->nicklist->host));
		new_free(&(tmp->nicklist->server));
		new_free((char **)&(tmp->nicklist));
		new_free(&(tmp->channel));
		new_free(&(tmp->server1));
		new_free(&(tmp->server2));
		*list = tmp->next;
		new_free((char **)&tmp);
		total++;
	}
	return total;
}

int
remove_oldest_whowas(list, timet, count)
WhowasList **list;
time_t timet;
int count;
{
	WhowasList *tmp = NULL;
	time_t t;
	int total = 0;

	/* if no ..count.. then remove ..time.. links */
	if (!count) {
		t = time(NULL);
		while (*list && ((*list)->time + timet) <= t) {
			tmp = *list;
			new_free(&(tmp->nicklist->nick));
			new_free(&(tmp->nicklist->host));
			new_free(&(tmp->nicklist->server));
			new_free((char **)&(tmp->nicklist));
			new_free(&(tmp->channel));
			new_free(&(tmp->server1));
			new_free(&(tmp->server2));
			*list = tmp->next;
			new_free((char **)&tmp);
			total++;
		}
	}
	else {
		while (*list && count) {
			tmp = *list;
			new_free(&(tmp->nicklist->nick));
			new_free(&(tmp->nicklist->host));
			new_free(&(tmp->nicklist->server));
			new_free((char **)&(tmp->nicklist));
			new_free(&(tmp->channel));
			new_free(&(tmp->server1));
			new_free(&(tmp->server2));
			*list = tmp->next;
			new_free((char **)&tmp);
			total++;
			count--;
		}
	}
	return total;
}

void
clean_whowas_list()
{
   whowas_userlist_count -= remove_oldest_whowas(&whowas_userlist_list, 20 * 60, 0);
   whowas_reg_count -= remove_oldest_whowas(&whowas_reg_list, 10 * 60, 0);
   remove_oldest_splitin_whowas(&whowas_splitin_list, 15 * 60);
}

/* Used to rehash whowas listings for new users */
void sync_whowas_adduser(added)
UserList *added;
{
WhowasList *tmp;
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) 
	{
		if (!tmp->nicklist)   continue;
		if (check_channel_match(added->channels, tmp->channel)) 
		{
			if (match(added->host, tmp->nicklist->host) && match(added->nick, tmp->nicklist->nick))
				tmp->nicklist->userlist = added;
		}
	}
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
		if (!tmp->nicklist)   continue;
		if (check_channel_match(added->channels, tmp->channel)) 
		{
			if (match(added->host, tmp->nicklist->host) && match(added->nick, tmp->nicklist->nick))
				tmp->nicklist->userlist = added;
		}
	}
}

/* Used to rehash whowas listings for removed userlist entries */
void sync_whowas_unuser(entry)
UserList *entry;
{
WhowasList *tmp;
   for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)            continue;
      if (!tmp->nicklist->userlist)  continue;
         if (!my_stricmp(tmp->nicklist->userlist->host, entry->host) && !my_stricmp(tmp->nicklist->userlist->nick, entry->nick) && 
            check_channel_match(tmp->nicklist->userlist->channels, entry->channels))
            tmp->nicklist->userlist = NULL;
   }
   for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)            continue;
      if (!tmp->nicklist->userlist)  continue;
         if (!my_stricmp(tmp->nicklist->userlist->host, entry->host) && !my_stricmp(tmp->nicklist->userlist->nick, entry->nick) && 
            check_channel_match(tmp->nicklist->userlist->channels, entry->channels))
            tmp->nicklist->userlist = NULL;
   }
}

/* Used to rehash whowas listings for new shitlist entries */
void sync_whowas_addshit(added)
ShitList *added;
{
WhowasList *tmp;
char user[BIG_BUFFER_SIZE];
   for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)   continue;
      if (check_channel_match(added->channels, tmp->channel)) {
         sprintf(user, "%s!%s", tmp->nicklist->nick, tmp->nicklist->host);
         if (match(added->filter, user))
            tmp->nicklist->shitlist = added;
      }
   }
   for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)   continue;
      if (check_channel_match(added->channels, tmp->channel)) {
         sprintf(user, "%s!%s", tmp->nicklist->nick, tmp->nicklist->host);
         if (match(added->filter, user))
            tmp->nicklist->shitlist = added;
      }
   }
}

/* Used to rehash whowas listings for removed shitlist entries */
void sync_whowas_unshit(entry)
ShitList *entry;
{
WhowasList *tmp;
   for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)            continue;
      if (!tmp->nicklist->shitlist)  continue;
         if (!my_stricmp(tmp->nicklist->shitlist->filter, entry->filter) &&
            check_channel_match(tmp->nicklist->shitlist->channels, entry->channels))
            tmp->nicklist->shitlist = NULL;
   }
   for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
      if (!tmp->nicklist)            continue;
      if (!tmp->nicklist->shitlist)  continue;
         if (!my_stricmp(tmp->nicklist->shitlist->filter, entry->filter) &&
            check_channel_match(tmp->nicklist->shitlist->channels, entry->channels))
            tmp->nicklist->shitlist = NULL;
   }
}


/* BELOW THIS MARK IS THE CHANNEL WHOWAS STUFF */

extern	WhowasChanList *
check_whowas_chan_buffer(channel, unlink)
char	*channel;
int	unlink;
{
	WhowasChanList *tmp, *last = NULL;

	for (tmp = whowas_chan_list; tmp; tmp = tmp->next) {
		if (!my_stricmp(tmp->channellist->channel, channel)) {
			if (unlink) {
				if (last)
					last->next = tmp->next;
				else
					whowas_chan_list = tmp->next;
				whowas_chan_count--;
			}
			return(tmp);
		}
		last = tmp;
	}
	return( NULL );
}

void
add_to_whowas_chan_buffer(channel)
ChannelList *channel;
{
	WhowasChanList *new;
	WhowasChanList **slot;

	if (whowas_chan_count >= whowas_chan_max) {
		whowas_chan_count -=
		   remove_oldest_chan_whowas(&whowas_chan_list, 0,
		   (whowas_chan_max + 1) - whowas_chan_count); 
	}
	new = (WhowasChanList *) new_malloc(sizeof(WhowasChanList));

	new->channellist = channel;
	new->time = time(NULL);

	/* we've created it, now put it in order */
	for (slot = &whowas_chan_list; *slot; slot = &(*slot)->next)
	{
		if ((*slot)->time > new->time)
			break;
	}
	new->next = *slot;
	*slot = new;
	whowas_chan_count++;
}

int
remove_oldest_chan_whowas(list, timet, count)
WhowasChanList **list;
time_t timet;
int count;
{
	WhowasChanList *tmp = NULL;
	time_t t;
	int total = 0;

	/* if no ..count.. then remove ..time.. links */
	if (!count) 
	{
		t = time(NULL);
		while (*list && ((*list)->time + timet) <= t) 
		{
			tmp = *list;
			new_free(&(tmp->channellist->channel));
			new_free(&(tmp->channellist->topic));
			new_free(&(tmp->channellist->modelock_key));
			tmp->channellist->nicks = NULL;
			tmp->channellist->bans = NULL;
			new_free((char **)&(tmp->channellist));
			*list = tmp->next;
			new_free((char **)&tmp);
			total++;
		}
	}
	else 
	{
		while (*list && count) 
		{
			tmp = *list;
			new_free(&(tmp->channellist->channel));
			new_free(&(tmp->channellist->topic));
			new_free(&(tmp->channellist->modelock_key));
			tmp->channellist->nicks = NULL;
			tmp->channellist->bans = NULL;
			new_free((char **)&(tmp->channellist));
			*list = tmp->next;
			new_free((char **)&tmp);
			total++;
			count--;
		}
	}
	return total;
}

void
clean_whowas_chan_list()
{
   whowas_chan_count -= remove_oldest_chan_whowas(&whowas_chan_list,
      24 * 60 * 60, 0);
}

WhowasList *find_in_whowas(char *nick)
{
WhowasList *tmp;
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next)
		if (tmp->nicklist && match(tmp->nicklist->nick, nick))
			return(tmp);
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next)
		if (tmp->nicklist && match(tmp->nicklist->nick, nick))
			return(tmp);
	return NULL;	
}

void show_whowas()
{
	int count = 1;
	WhowasList *tmp;

	say("Scanning whowas userlist, flag thinks there is %d entries", whowas_userlist_count);
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next) {
		if (!tmp->nicklist)
			continue;
		else
			say("%d: %s %s", count, tmp->nicklist->nick, tmp->nicklist->host);
		if (!tmp->channel)
			continue;
		else
			say("%d: %s", count, tmp->channel);
		count++;
	}
	count = 1;
	say("Scanning whowas reg, flag thinks there is %d entries", whowas_reg_count);
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next) {
		if (!tmp->nicklist)
			continue;
		else
			say("%d: %s %s", count, tmp->nicklist->nick, tmp->nicklist->host);
		if (!tmp->channel)
			continue;
		else
			say("%d: %s", count, tmp->channel);
		count++;
	}
}

void show_wholeft(char *channel)
{
	WhowasList *tmp;
	int count = 0;
	int hook = 0;
	time_t ltime;
	ltime = time(NULL);
	for (tmp = whowas_userlist_list; tmp; tmp = tmp->next)
		if (tmp->server1 && tmp->server2)
		{
			if (!count++ && (hook = do_hook(WHOLEFT_HEADER_LIST, "%s %s %s %d %s %s", "Nick", "Host", "Channel", "Time", "Server", "Server")))
				put_it("%s", convert_output_format(get_string_var(FORMAT_WHOLEFT_HEADER_VAR), NULL));
			if (do_hook(WHOLEFT_LIST, "%s %s %s %d %s %s", tmp->nicklist->nick, tmp->nicklist->host, tmp->channel, ltime-tmp->time, tmp->server1?tmp->server1:"Unknown", tmp->server2?tmp->server2:"Unknown"))
				put_it("%s", convert_output_format(get_string_var(FORMAT_WHOLEFT_USER_VAR), "%s %s %s %d %s", tmp->nicklist->nick, tmp->nicklist->host, tmp->channel, ltime-tmp->time, tmp->server1?tmp->server1:""));
		}
	for (tmp = whowas_reg_list; tmp; tmp = tmp->next)
		if (tmp->server1 && tmp->server2)
		{
			if (!count++ && (hook = do_hook(WHOLEFT_HEADER_LIST, "%s %s %s %d %s %s", "Nick", "Host", "Channel", "Time", "Server", "Server")))
				put_it("%s", convert_output_format(get_string_var(FORMAT_WHOLEFT_HEADER_VAR), NULL));
			if (do_hook(WHOLEFT_LIST, "%s %s %s %d %s %s", tmp->nicklist->nick, tmp->nicklist->host, tmp->channel, ltime-tmp->time, tmp->server1?tmp->server1:"Unknown", tmp->server2?tmp->server2:"Unknown"))
			{
				put_it("%s", convert_output_format(get_string_var(FORMAT_WHOLEFT_USER_VAR), "%s %s %s %d %s", tmp->nicklist->nick, tmp->nicklist->host, tmp->channel, ltime-tmp->time, tmp->server1?tmp->server1:""));
			}
		}
	if (count && hook && get_string_var(FORMAT_WHOLEFT_FOOTER_VAR))
		put_it("%s", convert_output_format(get_string_var(FORMAT_WHOLEFT_FOOTER_VAR), NULL));
}
